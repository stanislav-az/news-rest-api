-- TO DO
--ON DELETE, ON UPDATE
--Make a trigger which lowercases tags and categories

CREATE TABLE users(
    user_id serial PRIMARY KEY,
    name text NOT NULL,
    surname text NOT NULL, 
    avatar text, 
    date_created timestamp DEFAULT current_timestamp,
    is_admin boolean DEFAULT false
    );

CREATE TABLE authors(
    user_id integer PRIMARY KEY REFERENCES users,
    description text
    );

CREATE TABLE tags(
    tag_id serial PRIMARY KEY,
    name text NOT NULL,
    UNIQUE (name)
    );

CREATE TABLE categories(
    category_id serial PRIMARY KEY,
    name text,
    parent_id integer REFERENCES categories DEFAULT NULL,
    CHECK (parent_id <> category_id),
    UNIQUE (name)
);

CREATE TABLE news(
    news_id serial PRIMARY KEY,
    title text NOT NULL,
    date_created timestamp DEFAULT current_timestamp,
    user_id integer REFERENCES authors NOT NULL,
    category_id integer REFERENCES categories NOT NULL,
    content text,
    main_photo text,
    is_draft boolean DEFAULT true
);

CREATE TABLE tags_news(
    tag_id integer REFERENCES tags,
    news_id integer REFERENCES news,
    PRIMARY KEY (tag_id, news_id)
);

CREATE TABLE commentaries(
    commentary_id serial PRIMARY KEY,
    content text NOT NULL,
    news_id integer REFERENCES news NOT NULL
);