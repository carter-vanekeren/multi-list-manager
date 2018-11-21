(*-------------------------------DOCUMENT.ML------------------------------------
   -> Defines a "document" type which tracks a current state along with
   undo/redo history.  The document type is polymorphic meaning data can be any
   type such as 'int document' or 'string list document'.

    -> Document type declaration:
      - current -> represents current list of type 'a
      - undo_stack -> represents list of previous current lists
      - redo_stack -> represents list of previously undone current lists
------------------------------------------------------------------------------*)
type 'a document = {
    mutable current    : 'a ;
    mutable undo_stack : 'a list;
    mutable redo_stack : 'a list;
};;

(*---------------------------------FUNCTIONS----------------------------------*)

(*------------------------------------------------------------------------------
-------------------------------Instructor Notes---------------------------------
     val make : 'a -> 'a document
       -> Create a new document with initial as the current state and empty
        undo/redo stacks.
--------------------------------------------------------------------------------
--------------------------------Carter's Notes----------------------------------
     -> Creates a new document record with current = to initial and lets
     undo/stacks equal empty lists.
--------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
let make initial =

   {current = initial; undo_stack = []; redo_stack = []}
;;

(* -----------------------------------------------------------------------------
--------------------------------Instructor Notes--------------------------------
     val set : 'a document -> 'a -> unit
       -> Set the document to the given data. Push current state into the
       undo stack. Empty the redo stack.
--------------------------------------------------------------------------------
---------------------------------Carter's Notes---------------------------------
     -> Prepends undo_stack with the current list.  Sets current list to the new
     data.  Sets redo_stack to an empty list.
--------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
let set document data =
   document.undo_stack <- (document.current :: document.undo_stack);
   document.current <- data;
   document.redo_stack <- [];
;;
(* -----------------------------------------------------------------------------
--------------------------------Instructor Notes--------------------------------
      val undo : 'a document -> bool
        -> If the undo_stack is not empty, undo the last operation. current
        is moved to the redo_stack and the top element of undo_stack is
        removed and becomes current.  Returns true if changes are made
        and false if undo_stack is empty and no changes are made. Operates
        in constant time.
--------------------------------------------------------------------------------
---------------------------------Carter's Notes---------------------------------
      -> Case 1 - undo_stack is not empty:
        ->  Prepends redo_stack with current list.  Peels off head of undo_stack
        and sets it to the current list and sets the undo_stack to the remaining
        tail of the undo_stack. Returns true for success.
      -> Case 2 - undo_stack is empty:
        -> Returns false for failure
--------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
let undo document =
   if document.undo_stack != [] then
     begin
       document.redo_stack <- (document.current :: document.redo_stack);
       document.current <- List.hd (document.undo_stack);
       document.undo_stack <- List.tl (document.undo_stack);
       true
     end
    else false;
;;

(* -----------------------------------------------------------------------------
--------------------------------Instructor Notes--------------------------------
      val redo : 'a document -> bool
       -> If the redo_stack is not empty, redo the last operation. current
       is moved to the undo_stack and the top element of redo_stack is
       removed and becomes current.  Returns true if changes are made
       and false if redo_stack is empty and no changes are made. Operates
       in constant time.
--------------------------------------------------------------------------------
---------------------------------Carter's Notes----------------------------------
      -> Case 1 - uredo_stack is not empty
        ->  Prepends undo_stack with current list.  Peels off head of redo_stack
        and sets it to the current list and sets the redo_stack to the remaining
        tail of the redo_stack.
--------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
let redo document =
   if document.redo_stack != [] then
    begin
      document.undo_stack <- (document.current :: document.undo_stack);
      document.current <- List.hd (document.redo_stack);
      document.redo_stack <- List.tl (document.redo_stack);
      true
    end
  else false;
;;
