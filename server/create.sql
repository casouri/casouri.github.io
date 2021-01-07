create table view (
ip char(16),
code integer,
time text,
uri text,
primary key (ip, uri)
);

create table like (
ip char(16),
time text,
uri text,
primary key (ip, uri)
);
