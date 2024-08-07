Data Source
	We have used Mockaroo (Random Data generator) to generate the data. The data is obtained in CSV files, they were imported into tables using the import option.

Database
	The database can be created using the create.sql file. The data can be loaded into the database using the load.sql file.

create.sql
	This file has all the DDLs in it, along with alter table commands.
load.sql
	This can be used to load data into the databases. This has the insert commands for all the 8 tables.

1. Before running create.sql, we have to create a database without any constraints.
2. The SQL statements are designed in such a way that the content in the database is cleaned.

Tools Used
	PgAdmin4

User Interface
	We have used Python script to create the user interface for our database. We have created 5 templates each for each web page.

Libraries used:
	Flask
	Psycopg2

Python version: 3.6 or later