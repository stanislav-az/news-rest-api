CREATE TABLE courses(
    c_no text PRIMARY KEY,
    title text,
    hours integer
    );

INSERT INTO courses(c_no, title, hours)
    VALUES 
        ('CS301', 'Базы данных', 30),
        ('CS305', 'Сети ЭВМ', 60);

CREATE TABLE students(
    s_id integer PRIMARY KEY,
    name text,
    start_year integer
    );

INSERT INTO students(s_id, name, start_year)
    VALUES 
        (1451, 'Анна', 2014),
        (1432, 'Виктор', 2014),
        (1556, 'Нина', 2015);

CREATE TABLE exams(
    s_id integer REFERENCES students(s_id),
    c_no text REFERENCES courses(c_no),
    score integer,
    CONSTRAINT pk PRIMARY KEY(s_id, c_no)
    );

INSERT INTO exams(s_id, c_no, score)
    VALUES 
        (1451, 'CS301', 5),
        (1556, 'CS301', 5),
        (1451, 'CS305', 5),
        (1432, 'CS305', 4);
        