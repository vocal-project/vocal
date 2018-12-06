(** A fast, tail-recursive implementation of List.map.
    Based on the "Faster map" project by Anton Bachin, https://github.com/aantron/faster-map/
    Coq port and proof by Xavier Leroy, Inria, based on a suggestion by François Pottier. *)

Require Import List Bool Arith Omega Wf_nat.

(** Some additional lemmas about lists that are not in Coq's standard library. *)

Remark list_length_induction_principle:
  forall (T: Type) (P: list T -> Prop),
  (forall l, (forall l', length l' < length l -> P l') -> P l) ->
  forall l, P l.
Proof.
  intros T P HREC.
  assert (forall n, forall l, length l = n -> P l).
  { intros n0. pattern n0. apply lt_wf_ind. intros. apply HREC.
    intros. apply H with (length l'). omega. auto. }
  intros. apply H with (length l); auto.
Qed.

Remark firstn_short:
  forall (T: Type) n (l: list T), length l <= n -> firstn n l = l.
Proof.
  induction n; destruct l as [ | x t]; simpl; intros.
- auto.
- exfalso; omega.
- auto.
- rewrite IHn by omega. auto.
Qed.

Remark skipn_length:
  forall (T: Type) n (l: list T), length (skipn n l) = length l - n.
Proof.
  induction n; destruct l as [ | x t]; simpl; auto.
Qed.

(** This is the "blocking" factor for the list operations.  The original list is
  split into chunks of [N] elements, which are then processed.
  Bachin's code uses [N = 12].  Here we use [N = 4] because it produces easier-to-read code.
  Any positive value will do. *)

Notation N := 4%nat (only parsing).

(** A Notation is used instead of a Definition because Coq's guard condition for fixpoints
  does not unroll definitions, causing the [split] fixpoint below to fail. *)

Section MAP.

Context {A B: Type} (f: A -> B).

(** Skipping the first [n] elements of a list. *)

Section SPLIT1.

Context {C: Type} (k1: list A -> list A -> C) (k2: list A -> C).

Fixpoint split1 (n: nat) (l l': list A) : C :=
  match n with
  | O => k1 l l'
  | S n =>
      match l' with
      | nil => k2 l
      | _ :: l'' => split1 n l l''
      end
  end.

End SPLIT1.

(** Cutting the list [l] into chunks of length N, except perhaps for the last one. *)

Fixpoint split (chunks: list (list A)) (l: list A) {struct l} : list (list A) :=
  split1 (fun l tail => split (l :: chunks) tail) (fun l => l :: chunks) N l l.

Lemma unroll_split_long:
  forall l chunks, length l >= N -> split chunks l = split (l :: chunks) (skipn N l).
Proof.
  intros. repeat (destruct l; [simpl in H; exfalso; omega|idtac]).
  auto.
Qed.

Lemma unroll_split_short:
  forall l chunks, length l < N -> split chunks l = l :: chunks.
Proof.
  intros. repeat (destruct l; [reflexivity|idtac]).
  simpl in H; exfalso; omega.
Qed.

(** To specify the [split] function, it is useful to give a meaning to the lists of "chunks"
  it returns and it takes as arguments. 
- Each chunk is a suffix of the original list, but only the first [N] elements are meaningful; elements past [N] are ignored.
- The chunks are a decomposition of the original list.  Hence, concatenating the
  first [N] elements of each chunk re-constitutes the original list.
- The chunks are produced in reverse order, hence the list of chunks needs reversing.
*)

Definition denot_chunks (chunks: list (list A)) : list A :=
  concat (rev (map (firstn N) chunks)).

Lemma denot_chunks_cons:
  forall l chunks, denot_chunks (l :: chunks) = denot_chunks chunks ++ firstn N l.
Proof.
  unfold denot_chunks; intros. generalize (@firstn A 4). intros first.  
  simpl. rewrite concat_app. simpl. rewrite app_nil_r. auto.
Qed. 

(** We can now specify [split] and prove it correct. *)

Lemma split_spec: forall l chunks,
  denot_chunks (split chunks l) = denot_chunks chunks ++ l.
Proof.
  intros l0; pattern l0; apply list_length_induction_principle.
  intros l HREC chunks.
  destruct (dec_lt (length l) N).
- rewrite unroll_split_short by assumption.
  rewrite denot_chunks_cons, firstn_short by omega.
  auto.
- rewrite unroll_split_long by omega.
  rewrite HREC.
+ rewrite denot_chunks_cons, app_ass, firstn_skipn. auto.
+ rewrite skipn_length. omega.
Qed.

Lemma split_length_spec:
  forall l chunks,
  (forall chunk, In chunk chunks -> length chunk >= N) ->
  match split chunks l with
  | nil => True
  | hd :: tl => length hd < N /\ forall chunk, In chunk tl -> length chunk >= N
  end.
Proof.
  intros l0; pattern l0; apply list_length_induction_principle.
  intros l HREC chunks CHUNKS.
  destruct (dec_lt (length l) N).
- rewrite unroll_split_short by assumption.
  auto.
- rewrite unroll_split_long by omega.
  apply HREC.
  rewrite skipn_length. omega.
  intros chunk [P|P].
+ subst chunk. omega.
+ auto.
Qed.

(** Map [f] over the elements of [chunk], which is assumed to have at most [n] elements. *)

Fixpoint map_head_chunk (n: nat) (chunk: list A) : list B :=
  match n with
  | O => nil
  | S n =>
      match chunk with
      | nil => nil
      | x :: chunk => f x :: map_head_chunk n chunk
      end
  end.

Lemma map_head_chunk_spec:
  forall n chunk, length chunk <= n -> map_head_chunk n chunk = map f chunk.
Proof.
  induction n; destruct chunk as [ | x l ]; simpl; intros.
- auto.
- exfalso; omega.
- auto.
- f_equal. apply IHn. omega.
Qed.

(** Map [f] over the first [n] elements of [chunk], which is assumed to have at least [n] elements.
   Prepend the result to [suffix]. *)

Fixpoint map_tail_chunk (n: nat) (suffix: list B) (chunk: list A) :=
  match n with
  | O => suffix
  | S n =>
      match chunk with
      | nil => nil
      | x :: chunk => f x :: map_tail_chunk n suffix chunk
      end
  end.

Lemma map_tail_chunk_spec:
  forall n suffix chunk,
  length chunk >= n -> map_tail_chunk n suffix chunk = map f (firstn n chunk) ++ suffix.
Proof.
  induction n; intros suffix [ | x l ]; simpl; intros.
- auto.
- auto.
- exfalso; omega.
- rewrite IHn by omega. auto.
Qed.

(** Iterate [map_tail_chunks] over a list of full chunks (each chunk has at least [N] elements). *)

Fixpoint map_all_tail_chunks (suffix: list B) (chunks: list (list A)) : list B :=
  match chunks with
  | nil => suffix
  | chunk :: chunks => map_all_tail_chunks (map_tail_chunk N suffix chunk) chunks
  end.

Lemma map_all_tail_chunks_spec:
  forall chunks suffix,
  (forall chunk, In chunk chunks -> length chunk >= N) ->
  map_all_tail_chunks suffix chunks = map f (denot_chunks chunks) ++ suffix.
Proof.
  induction chunks as [ | chunk chunks ]; intros suffix LENGTHS.
- auto.
- simpl in LENGTHS. rewrite denot_chunks_cons, map_app, app_ass. 
  rewrite <- IHchunks by eauto.
  rewrite <- map_tail_chunk_spec by eauto. 
  reflexivity.
Qed.

(** Putting it all together. *)

Definition map1 (l: list A) : list B :=
  match split nil l with
  | nil => nil
  | first :: rest => map_all_tail_chunks (map_head_chunk N first) rest
  end.

Lemma map1_spec:
  forall l, map1 l = map f l.
Proof.
  intros. unfold map1.
  generalize (split_spec l nil) (split_length_spec l nil); intros S1 S2.
  destruct (split nil l) as [ | first rest ].
- simpl in S1. subst l. auto.
- destruct S2 as [S3 S4]. simpl; tauto.
  rewrite map_all_tail_chunks_spec by assumption.
  simpl in S1. rewrite <- S1. rewrite map_head_chunk_spec by omega.
  rewrite denot_chunks_cons, map_app. 
  rewrite firstn_all2 by omega. auto.
Qed.

(** Now we specialize the code to the specific value of [N]. *)

Definition map := Eval compute in map1.

(** It still computes [List.map] *)

Theorem map_spec: forall l, map l = List.map f l.
Proof map1_spec.

End MAP.

(** And it extracts to Caml code with by-block recursions similar to Bachin's code. *)

Require Import ExtrOcamlBasic.

Extraction map.

