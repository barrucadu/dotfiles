program CSVDatabase;
{ A simple database program operating on plaintext files. Databases are stored in the form:

    tablename
    field1,field2,field3
    value1,value2,value3

  ie: CSV with table names.
  As the databases are plaintext, compression works exceedingly well on them. }

Uses SysUtils;

{ Using linked lists to allow arbitrary number of tables, fields per table, and records per table. }
type
   plink    = ^linked;
   prec     = ^records;
   ptab     = ^table;
   pdb      = ^database;

   linked   = Record
		 value : String;
		 next  : plink;                      { Pointer to next linked record }
	      End;

   records  = Record
		 values	: plink;                     { Pointer to first value }
		 next	: prec;                      { Pointer to next record }
	      End;

   table    = Record
		 name	 : String;
		 fields	 : plink;                   { Pointer to first field value object }
		 records : prec;                    { Pointer to first record object }
		 nfield	 : Integer;                 { Number of fields }
		 nrecs	 : Integer;                 { Number of records }
		 next	 : ptab;                    { Pointer to next table record }
	      End;

   database = Record
		 fname	: String;
		 tables	: ptab;                      { Pointer to linked list of tables }
		 ntabs	: Integer;                   { Number of tables }
	      End;

var
   prompt : String;   { User input }
   db	  : database; { Can have one database open at a time, todo: allow multiple databases. }

Function getval (str, sep : String; index : Integer) : String; { Get a value from a delimited string }
var
   i, j	  : Integer; { Counters }
   tmpstr : String;  { Working string }
begin
   i      := 0;
   j      := 0;
   tmpstr := '';

   { Fix up these variables so this'll work }
   index := index + 1;
   str   := str + ',';

   { Find the INDEXth value }
   while (i < Length(str)) and (j < index) do
   begin
      if str[i] <> sep then
	 tmpstr := tmpstr + str[i]
      else
      begin
	 j := j + 1;

	 if j <> index then
	    tmpstr := '';
      end;

      i := i + 1;
   end;

   getval := tmpstr;
end;

Function numvals (str, sep : String) : Integer; { Get the number of values in a line }
var
   i, n : Integer; { Counters }
begin
   n := 0;

   for i := 0 to Length(str) do
      if str[i] = sep then
	 n := n + 1;

   numvals := n;
end;

Procedure filllinked (str : String; link : plink); { Fill a linked list }
var
   current : plink;   { Current list }
   i, n	   : Integer; { Counters }
begin
   n       := numvals(str, ','); { Get number of values in str }
   current := link;

   for i := 0 to n do
   begin
      current^.value := getval(str, ',', i);
      new(current^.next); { Add a link to the list }
      current := current^.next;
   end;
end;

Procedure readdb (fname	: String; db : pdb); { Read database fname into structures. }
var
   fh	 : TextFile; { File handler }
   tmpln : String;   { Temporary string }
   ctab	 : ptab;     { Current table }
   crec	 : prec;     { Current record }
begin
   db^.fname := fname;
   db^.ntabs := 0;

   new(db^.tables);
   ctab := db^.tables; { Set the current table to the first }
   ctab^.name := '';

   { Open file for reading }
   assignfile(fh, fname);
   reset(fh);

   while not eof(fh) do
   begin
      readln(fh, tmpln);

      if Length(tmpln) > 0 then
      begin
	 if numvals(tmpln, ',') = 0 then { Table name }
	 begin
	    if Length(ctab^.name) <> 0 then { New table }
	    begin
	       new(ctab^.next);
	       ctab := ctab^.next;
	       db^.ntabs := db^.ntabs + 1;
	    end;

	    ctab^.name   := tmpln;
	    ctab^.nfield := 0;
	    ctab^.nrecs  := -1; { This seems to be 1 out otherwise }

	    new(ctab^.fields);
	    new(ctab^.records);
	    crec := ctab^.records;
	 end
	 else
	    if ctab^.nfield = 0 then { Field headings }
	    begin
	       filllinked(tmpln, ctab^.fields);     { Fill the linked list }
	       ctab^.nfield := numvals(tmpln, ','); { Save the number of fields }
	    end
	    else { A record }
	    begin
	       filllinked(tmpln, crec^.values); { Fill the linked list }
	       new(crec^.next); { Add another record on }
	       crec := crec^.next;
	       ctab^.nrecs := ctab^.nrecs + 1; { Increment the number of records }
	    end
      end;
   end;

   closefile(fh);
end; { readdb }

Procedure writedb (db : database); { Write database to file }
var
   fh	   : TextFile; { File handler }
   i, j, k : Integer;  { Counters }
   ctab	   : ptab;     { Current table }
   clink   : plink;    { Current link }
   crec	   : prec;     { Current record }
begin
   { Open database file for writing }
   assignfile(fh, db.fname);
   rewrite(fh);

   ctab := db.tables;

   for i := 0 to db.ntabs do
   begin
      { Write table name }
      writeln(fh, ctab^.name);

      { Write field headings }
      clink := ctab^.fields;
      for j := 0 to ctab^.nfield do
      begin
	 write(fh, clink^.value);

	 if j < ctab^.nfield then
	    write(fh, ',');

	 clink := clink^.next;
      end;
      writeln(fh, '');

      { Write records }
      { Note: this messes up lines! Weird characters put at start. }
      crec := ctab^.records;
      for j := 0 to ctab^.nrecs do
      begin
	 clink := crec^.values;
	 for k := 0 to ctab^.nfield do
	 begin
	    write(fh, clink^.value);

	    if k < ctab^.nfield then
	       write(fh, ',');

	    clink := clink^.next;
	 end;
	 crec := crec^.next;
	 writeln(fh, '');
      end;

      ctab := ctab^.next;
   end;

   closefile(fh);
end; { writedb }

Function substr (str : String; start, count : Integer) : String; { Return count characters from str, starting at start }
var
   i : Integer; { Counter }
   s : String;  { Working string }
begin
   s := '';

   { Grab the substring }
   for i := start to (start + count - 1) do
      s := s + str[i];

   substr := s;
end;

Function strstr (needle, haystack : String) : Boolean; { Check if needle is in haystack }
var
   i	: Integer; { Counter }
   isin	: Boolean; { Whether needle is in haystack }
begin
   isin := FALSE;

   { Check if needle is in haystack, linear search }
   for i := 0 to Length(haystack) - Length(needle) do
      if not isin then
	 isin := (substr(haystack, i, Length(needle)) = needle);

   strstr := isin;
end;

Procedure dblist (db : database); { List tables and fields in the database }
var
   i, j	 : Integer; { Counters }
   ctab	 : ptab;    { Current table }
   clink : plink;   { Current link }
begin
   ctab := db.tables;

   for i := 0 to db.ntabs do
   begin
      { Write table name }
      write(ctab^.name, ': ');

      { Write field headings }
      clink := ctab^.fields;
      for j := 0 to ctab^.nfield do
      begin
	 write(clink^.value);

	 if j < ctab^.nfield then
	    write(', ');

	 clink := clink^.next;
      end;

      ctab := ctab^.next;
      writeln();
   end;
end;

Procedure dbselect (str	: String; db : database); { Search records: select from TABLE [where FIELD=VALUE] - only one condition, no ordering }
{ Todo: conditions }
var
   tname  : String;  { Table name }
   ctab	  : ptab;    { Current table }
   crec	  : prec;    { Current record }
   clink  : plink;   { Current link }
   recs	  : Integer; { Counter }
   i, j	  : Integer; { Counters }
begin
   tname := getval(str, ' ', 2);

   { Find the given table }
   i    := 0;
   ctab := db.tables;
   while (ctab^.name <> tname) and (i < db.ntabs) do
   begin
      if ctab^.name <> tname then
	 ctab := ctab^.next;
      i := i + 1;
   end;

   if ctab^.name <> tname then
      writeln('Bad table name "', tname, '".')
   else
   begin
      recs := 0;
      crec := ctab^.records;

      { Print records }
      for i := 0 to ctab^.nrecs do
      begin
	 clink := crec^.values;
	 for j := 0 to ctab^.nfield do
	 begin
	    write(clink^.value);

	    if j < ctab^.nfield then
	       write(', ');

	    clink := clink^.next;
	 end;

	 crec := crec^.next;
	 recs := recs + 1;
	 writeln();
      end;

      writeln(recs, ' records returned.');
   end;
end;

Procedure dbinsert (str : String; db : pdb); { Insert records: insert into TABLE VALUES - all fields must be given }
begin
end;

Procedure dbdelete (str	: String; db : pdb); { Delete records: delete from TABLE [where FIELD=VALUE] - only one condition }
begin
end;

Procedure dbupdate (str	: String; db : pdb); { Update records: update TABLE set FIELD=NEWVALUE [where FIELD=VALUE] - only one condition, only one update }
begin
end;

Procedure doprompt (prompt : String; db : pdb); { Parse a prompt line and update the db object }
begin
   if prompt = 'sync' then { Sync database to file }
   begin
      writedb(db^);
      writeln('Written database to file');
   end
   else
      if prompt = 'list' then { List tables and fields }
	 dblist(db^)
      else
	 if substr(prompt, 1, 6) = 'select' then
	    dbselect(prompt, db^)
	 else
	    if substr(prompt, 1, 6) = 'insert' then
	       dbinsert(prompt, db)
	    else
	       if substr(prompt, 1, 6) = 'delete' then
		  dbdelete(prompt, db)
	       else
		  if substr(prompt, 1, 6) = 'update' then
		     dbupdate(prompt, db)
		  else
		     writeln('Bad command: ', prompt, ' ', substr(prompt, 1, 6), '-');
end; { doprompt }

begin
   if ParamCount <> 1 then
   begin
      writeln('Usage: csv-database [filename]');
      exit;
   end;

   readdb(ParamStr(1), @db);
   writeln('Read database from file');

   prompt := '';
   while prompt <> 'exit' do
   begin
      write(' > ');
      readln(prompt);

      if (prompt <> 'exit') and (prompt <> '') then
	 doprompt(prompt, @db);
   end;

   writedb(db);
   writeln('Written database to file');
end.