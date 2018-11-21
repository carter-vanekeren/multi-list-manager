open Document;;
open Doccol;;
open Printf;;
(*---------------------------------BULKOPS.ml-----------------------------------
   -> Implement bulk operations on Doccol's of string list
     -> Documents that are useful for multimanager.  Since the functions in
     this module require access to fields and types of other modules, start
     the file by opening those two modules:
       - open Document;;
       - open Doccol;;
     -> Also opens Printf foe formatted printing functionality in print showall
------------------------------------------------------------------------------*)

(*+++++++++++++++++++++++++++++++FUNCTIONS++++++++++++++++++++++++++++++++++++*)

(*------------------------------------------------------------------------------
-------------------------------Instructor Notes---------------------------------
   val showall : string list Doccol.doccol -> unit
     -> Prints all documents in doccol to the screen. For each list,
     prints the list name first and then each element of the list using
     Sortedlist functions. Uses higher-order functions to iterate over
     the doclist.

     EXAMPLE:
     --List test-data/heros.txt--
     Asami
     Bolin
     Bumi
     Jinora
     Korra
     Kya
     Mako
     Tenzin

     --List test-data/villains.txt--
     Amon
     Hiroshi
     Kuvira
     Ming-Hua
     P-li
     Unalaq
     Zaheer

     --List default.txt--
     Korra
     Meelo
     Pema
--------------------------------------------------------------------------------
--------------------------------Carter's Notes----------------------------------
     -> Defines print helper function that takes in a (string * 'a document') and
     prints name and then prints every element in the current list of doc.
     Passes print helper to List module function iter to visit every element in
     the given doccol's doclist.
--------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
let showall doccol =
  let print (name, doc) =
    printf "--List %s--\n" name;
    Sortedlist.print doc.current;
  in
  List.iter print doccol.docs;
  printf "\n";
;;

(*------------------------------------------------------------------------------
-------------------------------Instructor Notes---------------------------------
   val saveall : string list Doccol.doccol -> unit
     -> Saves all documents in doccol. Makes use of Util functions to do
     I/O. Makes use of higher-order functions to write each list to
     associated file name.
--------------------------------------------------------------------------------
--------------------------------Carter's Notes----------------------------------
     -> Defines helper function save that takes a (string * 'a document) value
     and uses Util module function strlist_to_file that writes the current list
     of the given document to the given filename. Uses List module function iter
     to visit every element in the given doccol's doclist and apply save
     function.
--------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
let saveall doccol =
    let save (name, doc) = Util.strlist_to_file doc.current name
    in
    List.iter save doccol.docs

;;

(*------------------------------------------------------------------------------
-------------------------------Instructor Notes---------------------------------
   val addall : 'a list Doccol.doccol -> 'a -> unit
     -> Adds the given element to all docs in doccol. Makes use of
     higher-order functions and Sortedlist functions to modify each
     list. Each doc/list can individually undo the addition.
--------------------------------------------------------------------------------
--------------------------------Carter's Notes----------------------------------
     -> Follows same structure as saveall and showall functions. Defines add
     helper function that takes in a pair and sets the passed document to the
     result produced by the Sortedlist module insert function whe passed the
     documents current list and the passed element to add.  Then uses List
     module function iter to visit and apply add function to every element in
     the given doccol's doclist.
--------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
let addall doccol elem =
   let add (name, doc) =
      set doc (Sortedlist.insert doc.current elem)
   in
   List.iter add doccol.docs
;;

(*------------------------------------------------------------------------------
-------------------------------Instructor Notes---------------------------------
   val mergeall : 'a list Doccol.doccol -> 'a list
     -> Merges all lists in doccol.docs into a single list and returns
     it. Uses higher-order functions and Sortedlist functions to perform
     the merge.
--------------------------------------------------------------------------------
--------------------------------Carter's Notes----------------------------------
     -> Defines helper1 function to retrieve key-value binding of every element
     of the given doccol's doclist.  Sets doclist variable to the result
     produced by the List module map function - a list of 'a document types.
     Then defines recursive merge function that uses SortedList module function
     merge on the created doclist variable.
--------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
let mergeall doccol =
   let helper1 (name,doc) = doc in
   let doclist = List.map helper1 doccol.docs in

   let rec merge doclist =
    match doclist with
    | [] -> []
    | head :: tail -> Sortedlist.merge head.current (merge tail)
   in
   merge doclist;
;;
