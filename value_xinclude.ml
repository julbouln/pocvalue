(*
    pocvalue - data manager
    Copyright (C) 2005 POC 

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

(** Really quick hack to process xinclude in xml. Only 2 functions to process from file and from string. Depends on libxml2.
*)

(** xinclude_process_file file open file, process xinclude and return the new xml data as string *)
external xinclude_process_file : string->string = "xml_xinclude_process_file";;
(** xinclude_process_file str url process str xinclude using url and return the new xml data as string *)
external xinclude_process_string : string->string->string = "xml_xinclude_process_string";;
