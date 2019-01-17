-- NOT NULL, DEFAULT, ON DELETE, ON UPDATE

CREATE TABLE users(
    user_id serial PRIMARY KEY,
    name text NOT NULL,
    surname text NOT NULL, 
    avatar text, 
    date_created timestamp, -- NOT NULL
    is_admin boolean DEFAULT false
    );

-- INSERT INTO courses(c_no, title, hours)
--     VALUES 
--         ('CS301', 'Базы данных', 30),
--         ('CS305', 'Сети ЭВМ', 60);

CREATE TABLE authors(
    author_id serial PRIMARY KEY,
    user_id integer REFERENCES users NOT NULL,
    description text
    );

-- INSERT INTO students(s_id, name, start_year)
--     VALUES 
--         (1451, 'Анна', 2014),
--         (1432, 'Виктор', 2014),
--         (1556, 'Нина', 2015);

CREATE TABLE tags(
    tag_id serial PRIMARY KEY,
    name text
    );

-- INSERT INTO exams(s_id, c_no, score)
--     VALUES 
--         (1451, 'CS301', 5),
--         (1556, 'CS301', 5),
--         (1451, 'CS305', 5),
--         (1432, 'CS305', 4);

CREATE TABLE categories(
    category_id serial PRIMARY KEY,
    name text,
    parent_id integer REFERENCES categories
);

CREATE TABLE news(
    news_id serial PRIMARY KEY,
    title text,
    date_created timestamp,
    author_id integer REFERENCES authors,
    category_id integer REFERENCES categories,
    content text,
    main_photo text,
    is_draft boolean
);

CREATE TABLE tags_news(
    tag_id integer REFERENCES tags,
    news_id integer REFERENCES news,
    PRIMARY KEY (tag_id, news_id)
);

CREATE TABLE commentaries(
    commentary_id serial PRIMARY KEY,
    content text NOT NULL,
    news_id integer REFERENCES news
);