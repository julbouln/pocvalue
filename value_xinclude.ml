
(** Really quick hack to process xinclude in xml. Only 2 functions to process from file and from string. Depends on libxml2.
*)

(** xinclude_process_file file open file, process xinclude and return the new xml data as string *)
external xinclude_process_file : string->string = "xml_xinclude_process_file";;
(** xinclude_process_file str url process str xinclude using url and return the new xml data as string *)
external xinclude_process_string : string->string->string = "xml_xinclude_process_string";;
