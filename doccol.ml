open Printf;;
(*---------------------------------DOCCOL.ml------------------------------------
  -> Type and functions for a collection of named documents.
   -> Tracks a current document and its name along with an association
   list of all docs in the collection.  Preserves uniqueness of names
   in the collection. Makes use of built-in List functions to
   ad/remove/get docs from the association list.

   -> Opens Printf module for formatted 'a to string conversion functionality

   -> Type to track a collection of named documents in an association
   list.
   - count -> count of docs in list
   - curdoc -> current list being edited
   - curname -> name of current list
   - docs -> association list of names/docs
------------------------------------------------------------------------------*)
type 'a doccol = {
  mutable count   : int;
  mutable curdoc  : 'a Document.document;
  mutable curname : string;
  mutable docs    : (string * 'a Document.document) list;
};;

(*---------------------------------FUNCTIONS----------------------------------*)

(*------------------------------------------------------------------------------
-------------------------------Instructor Notes---------------------------------
     val make : string -> 'a Document.document -> 'a doccol
       -> Create a doccol. The parameters name and doc become the current
       doc and the only pair in the docs association list.
--------------------------------------------------------------------------------
--------------------------------Carter's Notes----------------------------------
     -> Creates a new doccol record with count = 1, curdoc set to the passed in
     document, name set to the passed in name, and sets docs to a list with the
     singular pair-element (name, doc).
--------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
let make name doc =
   {count = 1; curdoc = doc; curname = name; docs = (name, doc) :: []}
;;

(*------------------------------------------------------------------------------
-------------------------------Instructor Notes---------------------------------
     val add : 'a doccol -> string -> 'a Document.document -> bool
       -> If there is already a doc with name in doccol, do nothing and
       return false.  Otherwise, add the given doc to doccol with the
       given name, update the count of docs and return true. Uses
       association list functions from the List module.
--------------------------------------------------------------------------------
--------------------------------Carter's Notes----------------------------------
     -> Case 1 - Name of passed document is already in the given doccol
      -> Returns false for failure to add
     -> Case 2 - Name of passed document is not already in given doccol
      -> Prepends the given doccol's doclist with pair (name, doc).  Increments
      count and returns true for success.
--------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
let add doccol name doc =
   if (List.mem_assoc name doccol.docs) then
      false
    else begin
      doccol.docs <- (name, doc) :: doccol.docs;
      doccol.count <- doccol.count + 1;
      true
    end
;;

(*------------------------------------------------------------------------------
-------------------------------Instructor Notes---------------------------------
     val remove : 'a doccol -> string -> bool
       -> If name is equal to curname for the doccol, do nothing and return
       false.  If there is no doc with name in doccol, do nothing and
       return false.  Otherwise, remove the named doc from doccol,
       decrement the count of docs, and return true. Uses association list
       functions from the List module.
--------------------------------------------------------------------------------
--------------------------------Carter's Notes----------------------------------
     -> Case 1 - Name is equal to the current document's name
      -> Returns false for failure
     -> Case 2 - Name is found in doccol's doclist
      -> Uses list module function remove_assoc to remove the (name, doc) element
      and sets doccol's doc list to the new list returned by remove_assoc.
      Decrements count and returns true for success.
     -> Case 3 - Name of passed document is not in given doccol
      -> Returns false for failure
--------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
let remove doccol name =
   if name = doccol.curname then false
   else if (List.mem_assoc name doccol.docs) then
    begin
      doccol.docs <- List.remove_assoc name doccol.docs;
      doccol.count <- doccol.count - 1;
      true;
    end
    else false
  ;;

(*------------------------------------------------------------------------------
-------------------------------Instructor Notes---------------------------------
    val has : 'a doccol -> string -> bool
      -> Returns true if the named doc is in the doccol and false otherwise.
--------------------------------------------------------------------------------
--------------------------------Carter's Notes----------------------------------
      -> Returns value returned by List module function mem_assoc when passed
      given name and doccol's doclist.
--------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
let has doccol name =
   List.mem_assoc name doccol.docs
;;

(*------------------------------------------------------------------------------
-------------------------------Instructor Notes---------------------------------
    val switch : 'a doccol -> string -> bool
      -> Change the current document/name to the named document and return
      true. If the named document does not exist, return false and make
      no changes to doccol.
--------------------------------------------------------------------------------
--------------------------------Carter's Notes----------------------------------
    -> Case 1 - Passed name is found in doccol's doclist
      -> Sets current name and current doc to the passed name and its associated
      document value. Returns true for success.
    -> Case 2 - passed name is not found in doccol's doclist
      -> Returns false for failure
--------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
let switch doccol name =
   if List.mem_assoc name doccol.docs then
    begin
      doccol.curname <- name;
      doccol.curdoc <- List.assoc name doccol.docs;
      true
    end
    else false
;;

(*------------------------------------------------------------------------------
-------------------------------Instructor Notes---------------------------------
   val string_of_col : 'a doccol -> string
     -> Creates a string representation of doccol showing the count of
     docs and the names of all docs. Each doc is listed on its own
     line. It has the following format:

     4 docs
     - test-dir/heros.txt
     - places.txt
     - stuff.txt
     - default.txt

     Does not define any helper functions. Makes use of higher order
     functions such as List.map and/or List.fold. May also use string
     processing functions such asString.concat and/or Printf.sprintf *
--------------------------------------------------------------------------------
--------------------------------Carter's Notes----------------------------------
    -> First prints number of documents by accesing doccol's count variable.
    Then, defines getfirst function to obtain name element of each doccol.docs
    element. Uses List module function map passing in getfirst function and
    doccol's doclist and to obtain a list of the doccol;s document names and
    sets result to namelst variable.  Creates a string that concatenates the
    strcount variable (number of docs) and the strnames variable (the list of
    names conactenated together and seperated by a "/n- " by using the
    String.concat function on namelst)
--------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
let string_of_doccol doccol =
   let strcount = sprintf "%d docs\n" doccol.count in
   let getfirst (k,v) = k in
   let namelst = List.map getfirst doccol.docs in
   let strnames = String.concat "\n- " namelst in
   strcount ^ "- " ^ strnames

  ;;
