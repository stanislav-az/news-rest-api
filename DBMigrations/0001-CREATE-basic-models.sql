-- TO DO
--ON DELETE, ON UPDATE

CREATE TABLE users(
    id serial PRIMARY KEY,
    name text NOT NULL,
    surname text NOT NULL, 
    avatar text, 
    date_created timestamp DEFAULT current_timestamp,
    is_admin boolean DEFAULT false
);

CREATE TABLE authors(
    id serial PRIMARY KEY,
    user_id integer,
    description text,
    FOREIGN KEY (user_id) REFERENCES users (id),
    UNIQUE (user_id)
);

CREATE TABLE tags(
    id serial PRIMARY KEY,
    name text NOT NULL,
    UNIQUE (name)
);

CREATE TABLE categories(
    id serial PRIMARY KEY,
    name text,
    parent_id integer REFERENCES categories DEFAULT NULL,
    CHECK (parent_id <> id),
    UNIQUE (name)
);

CREATE TABLE news(
    id serial PRIMARY KEY,
    title text NOT NULL,
    date_created timestamp DEFAULT current_timestamp,
    author_id integer,
    category_id integer,
    content text,
    main_photo text,
    is_draft boolean DEFAULT true,
    FOREIGN KEY (author_id) REFERENCES authors (id),
    FOREIGN KEY (category_id) REFERENCES categories (id)
);

CREATE TABLE tags_news(
    tag_id integer,
    news_id integer,
    PRIMARY KEY (tag_id, news_id),
    FOREIGN KEY (tag_id) REFERENCES tags (id),
    FOREIGN KEY (news_id) REFERENCES news (id)
);

CREATE TABLE commentaries(
    id serial PRIMARY KEY,
    content text NOT NULL,
    news_id integer REFERENCES news NOT NULL,
    FOREIGN KEY (news_id) REFERENCES news (id)
);