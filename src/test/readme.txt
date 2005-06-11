 
              Testing

For every kind of database there is a file, named according to
the database.
Currently these are:

test-oracle.lisp  
test-sql-server.lisp 
test-mysql.lisp

A file contains tests for the database.
Each file has its own package, named according to the database/file,
i.e. "TEST-ORACLE", "TEST-SQL-SERVER", "TEST-MYSQL".

The tests are started with the function run-all-tests, which takes
as a parameter a odbc connection, for example:

[44]> (setf *con* (connect-oracle "ltrav1" "scott" "tiger"))

#<ODBC-CONNECTION SERVER="ltrav1" DBMS="Oracle" USER="scott">
[45]>  (test-oracle:run-all-tests *con*)

NIL


