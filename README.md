# PTQL

PTQL (Plain Text Query Language) is an exercise project that I'm using to
help me in learning Common Lisp. PTQL implements an SQL like dialect in Lisp
that is can be used to query plain text files (currently only CSV files are
supported). This project is a work in progress.

## Running the program

To run the program, use the SBCL implementation (There's a warning suppression
macro which uses SBCL specific stuff, apart from that the program should run
fine on any other implementation). I`ll try to make it portable some other time.

```shell
sudo pacman -Sy sbcl
```

Also, you must install ASDF. It may come installed with your implementation.

I recommend installing quicklisp. Their site contains precise
[instructions](https://www.quicklisp.org/beta/).

Clone the repo, then run Make on the repo main folder.

I also recommend installing `rlwrap`, because the default input sucks.
This is optional, but the program is pretty lame without it.

```shell
cd <your-folder>/ptql
make
cd bin
rlwrap ptql
```

## Querying text files

Suppose we had this file:

```csv
id,field,extra
1,joe,4
2,moe,4
3,doe,5
```

Import text files using `import-table`:

```lisp
import-table #p"/home/user/file.csv" table
```

The file header column names will be used to refer to the columns.

Then, you may select from the table:

```lisp
select * :from table
```

The results will be printed on the terminal.

Current, only `where`, `order-by` and `limit` are supported:

```lisp
select (id field) :from table :where (> (length field) 5) :order-by (extra) :limit 10
```

You can use any valid Lisp expression on the `where` clause. On the `order-by`,
use a list with the column names. You may order by descending as well:

```lisp
select (field) :from table :order-by (name (extra desc))
```

Column names may also have white space. If it is so, refer to them by enclosing
the name with `|`:

```lisp
select (|person name|) :from some-other-table :where (= 90 |person weight|)
```

## Other commands

There are no other commands (yet). Enter `help` to get a tip of the syntax.
I'll be adding new stuff from time to time, although the current focus is to
make the basic querying functionality as robust as possible.
