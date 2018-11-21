(* multimanager.ml : main function to allow manipulation of multiple
   lists of sorted, unique elements.  *)

open Printf;;

(* Help string to be printed for the "help" command. *)
let help_string =
  let lines = [
      "MULTI MANAGER";
      "Maintains multiple sorted lists of unique elements..";
      "";
      "--PROGRAM COMMANDS--:";
      "  help           : print this help message";
      "  quit           : quit the program";
      "";
      "--CURRENT LIST COMMANDS--";
      "The following commands modify the current list";
      "  show           : print the current list to the screen";
      "  clear          : set the list to empty, preserves undo history";
      "  add <elem>     : add elem to the list";
      "  remove <elem>  : remove elem from list";
      "  mergein <file> : load the sorted list in the named file and merge with current list (undoable)";
      "  save           : save the current list using the name of the list as the save file";
      "  saveas <file>  : save the current list to the given file name; keeps the list name the same";
      "  undo           : undo the last operation restoring list to a previous state";
      "  redo           : redo the last undone operation restoring list to a previous state";
      "";
      "--LIST MANAGEMENT COMMANDS--";
      "The following commands will fail if a list name is already in use (new/open) or no present (close/edit/merge)";
      "  lists          : prints the lists that are currently open";
      "  edit <list>    : set the named list to the current list";
      "  new <list>     : create a new empty list and switch to it";
      "  open <file>    : create a new list named after the file specified; load the contents of the file into it and switch to it";
      "  close <list>   : close the list with given name and remove it from the open documents; cannot close the current list";
      "  merge <list>   : merge the named list contents into the current list";
      "";
      "--BULK OPERATIONS--";
      "The following commands act upon all open lists";
      "  showall        : print all lists labelled with their list name";
      "  saveall        : save all open lists; use filenames identical the list names (not undoable)";
      "  addall <elem>  : add elem to all open lists; each list can undo this individually";
      "  mergeall       : merge the contents of all lists into the current list; undoable";
    ] in
  String.concat "\n" lines
;;

(* Tracks the global state associated with the application. This
   binding uses a series of statements to initialize the global state
   to have a default empty list named 'default.txt' which is the
   current document and the only entry in the doccol. *)
let global : string list Doccol.doccol =
  let default_doc = Document.make [] in
  let default_name = "default.txt" in
  Doccol.make default_name default_doc
;;

(* Set to true to end execution of the program *)
let quit_now = ref false;;


(*------------------------------------------------------------------------------
-------------------------------Instructor Notes---------------------------------
   val execute_command : string array -> unit
     -> Execute a single command which is the 0th element of the argument
     array tokens.  If the command has additional parameters these will
     be in tokens.(1), tokens.(2), etc.  Makes use of functions in Util,
     Sortedlist, Document, doccol, and Bulkops to implement each
     command.
--------------------------------------------------------------------------------
--------------------------------Carter's Notes----------------------------------
    -> Uses Pattern matching to match tokens with a sideffect.
      - NOTE: I will not describe help, quit, show, and clear because they were implemented
      by instructor.
      - NOTE: All current list comands set the current document of the global
      variable to the result produced by functions from Sortedlist and Util,
      with the exception of "save" and "saveas".  I will refer to "current
      document of the global variable" as 'gcd' in my comments.

      -> CURRENT LIST COMMANDS--------------------------------------------------

        -> "add" - Sets gcd to result produced by using Sortedlist module
        function, insert, on second token element and the gcd's current list.

        -> "remove" - Sets gcd to to the result produced by the SortedList
        module function, remove, on second token element and the gcd's current
        list.

        -> "save" - Uses Util module function, strlist_to_file, to write gcd's
        current list to the file referred to by gcd' current name.

        -> "saveas" - Uses Util module function, strlist_to_file, to write gcd's
        current list to the file referred to by the second element in tokens.

        -> "load" - Sets the gcd to the result produced by Util module function,
        strlist_from_file, the list produced from the filename referred to by
        the secnod element of tokens.

        -> "mergein" - Sets the gcd to the result produced by the SortedList
        module function, merge, on the gcd's current list and the list produced
        by using the Util module function, strlist_from_file, on the second
        element of tokens.

        -> "undo" - Sets the gcd to the previous version of gcd if possible, if
        not, prints error message.

        -> "redo" - Sets the gcd to the latest list to be undone if possible, if
        not, then prints error message.

      -> LIST MANAGEMENT COMMANDS-----------------------------------------------

        -> "lists" - prints the string produced by the Doccol string_of_doccol
        function with global being the passed paramter.

        -> "edit" - Switches the gcd to to the document name referred to by
        the second element of tokens.  Uses ignore to maintain unit return type.
        If the document name provided is not in global, then print error message

        -> "new" - If the document name referred to by the second element of
        tokens does not already exist in the global doccol, then creates a
        document that has a current list of an empty list, adds it to global
        and switches the gcd to the new document.  Uses ignore to maintain unit
        return type. In case where document name is already in global, then
        print error message.

        -> "open" - If the document name referred to by the second element of
        tokens does not already exist in the global doccol, then creates a
        document that has a current list of the contents referred to by the
        second element in tokens.  Adds the new document to global and switches
        the gcd to this new document. Uses ignore to maintain unit
        return type. In case where document name is already in global, then
        print error message.

        -> "close" - If the document name referred to by the second element of
        tokens does not already exists in the global doccol and it is not the
        name of the gcd, then remove the document from global.  Else, print
        error message.

        -> "merge" - If the document name referred to by the second element of
        tokens is found in global, merge gcd's current list with the current
        list of associated value of the document name.  Uses List module function
        assoc to retrieve value associated with the given name.  If the document
        is not found in global, print error message.

      -> BULK OPERATIONS--------------------------------------------------------

        -> All tokens are carried out by simply calling the associated Bulkops
        function on the global variable.  In the case of addall, the element
        being added is referred to by the second element of token.
--------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
let rec execute_command tokens =
  let cmd = tokens.(0) in       (* 0th element is command *)
  match cmd with
  (* ---PROGRAM COMMANDS-- *)
  | "help" ->
     printf "%s\n" help_string;
  | "quit" ->
     quit_now := true;

  (* --CURRENT LIST COMMANDS-- *)
  | "show" ->
     printf "--BEG LIST--\n";
     Sortedlist.print global.curdoc.current;
     printf "--END LIST--\n";
  | "clear" ->
     Document.set global.curdoc [];
  | "add" ->
      Document.set global.curdoc (Sortedlist.insert global.curdoc.current tokens.(1))
  | "remove" ->
     Document.set global.curdoc (Sortedlist.remove global.curdoc.current tokens.(1))
  | "save" ->
     Util.strlist_to_file global.curdoc.current global.curname
  | "saveas" ->
     Util.strlist_to_file global.curdoc.current tokens.(1)
  | "load" ->
     Document.set global.curdoc (Util.strlist_from_file tokens.(1))
  | "mergein" ->
      let mergee = Util.strlist_from_file tokens.(1) in
      Document.set global.curdoc (Sortedlist.merge global.curdoc.current mergee)
  | "undo" ->
     if Document.undo global.curdoc = false then
        printf "WARNING: undo list empty, no changes made\n";
  | "redo" ->
     if Document.redo global.curdoc = false then
        printf "WARNING: redo list empty, no changes made\n";

  (* --LIST MANAGEMENT COMMANDS-- *)
  | "lists" ->
     printf "%s\n" (Doccol.string_of_doccol global)
  | "edit" ->
     if Doccol.has global tokens.(1) then
        ignore(Doccol.switch global tokens.(1))
     else printf "ERROR: list '%s' does not exist\n" tokens.(1)
  | "new" ->
      if Doccol.has global tokens.(1) then
        printf "ERROR: list '%s' already exists\n" tokens.(1)
      else begin
        let newdoc = Document.make [] in
        ignore(Doccol.add global tokens.(1) newdoc);
        ignore(Doccol.switch global tokens.(1));
      end
  | "open" ->
     let contains = Doccol.has global tokens.(1) in
     if contains then
        printf "ERROR: list '%s' already exists\n" tokens.(1)
     else begin
       let initial = Util.strlist_from_file tokens.(1) in
       ignore(Doccol.add global tokens.(1) (Document.make initial));
       ignore(Doccol.switch global tokens.(1));
     end
  | "close" ->
     if tokens.(1) = global.curname then
        printf "ERROR: cannot close the current list\n"
     else if Doccol.remove global tokens.(1) then ()
     else printf "ERROR: list '%s' does not exist\n" tokens.(1)

  | "merge" ->
     if Doccol.has global tokens.(1) then
        begin
          let mergee = List.assoc tokens.(1) global.docs in
          Document.set global.curdoc (Sortedlist.merge global.curdoc.current mergee.current)
        end
     else printf "ERROR: list '%s' does not exist\n" tokens.(1);


  (* --BULK OPERATIONS-- *)
  | "showall" ->
     Bulkops.showall global;
  | "saveall" ->
     Bulkops.saveall global;
  | "addall" ->
     Bulkops.addall global tokens.(1);
  | "mergeall" ->
     Document.set global.curdoc (Bulkops.mergeall global);

  (* Catch-all *)
  | _ ->
     printf "Unknown command '%s'\n" tokens.(0)
;;

(*********************************************************************************
   Code beyond this point should not require modification though it
   may be interesting to examine.
*)
let echo  = ref false;;         (* command echoing on/off  *)
let debug = ref false;;         (* turn on/off debug printing *)

(* Options accepted by the program *)
let options = Arg.([
  ("-echo",  Set(echo),  "Turn on command echoing (default: off)");
  ("-debug", Set(debug), "Turn on debug printing  (default: off)");
]);;

(* Do nothing with extra command line arguments *)
let handle_extra_args arg = ();;

(* Simple usage message for Arg.parse *)
let usage = sprintf "usage: %s [options]" Sys.argv.(0);;

(* main routine *)
let _ =
  Arg.parse options handle_extra_args usage;    (* parse command line options *)
  begin try
      while !quit_now = false do                (* loop until quit command is issued *)
        printf "(%s)> " global.curname;         (* print prompt *)
        let line = read_line () in              (* read a line of input from stdin *)
        if !echo then                           (* if echoing is on, print the line *)
          printf "%s\n" line;
        let tokens =                            (* split line into tokens on spaces *)
          Array.of_list (Str.split (Str.regexp " +") line) in
        let ntok = Array.length tokens in
        if !debug then                          (* possibly print debuggin info on tokens *)
          begin
            printf "'%s' has %d tokens\n" line ntok;
            for i=0 to ntok-1 do
              printf "%d : %s\n" i tokens.(i);
            done
          end;
        if ntok>0 then
          try execute_command tokens;           (* execute a command *)
          with e ->                             (* out of bounds access, file not found, etc. doesn't kill program *)
            let excstring = Printexc.to_string e in
            printf "Error with '%s': %s\n" line excstring;
      done;
    with
    | End_of_file -> ()                         (* end of input reached *)
  end;
  printf "\nLists multi-managed!\n";
;;
