CREATE VIEW searchable_news AS (
  WITH np AS (
    SELECT n.*, count(*) photos_num
    FROM news n
    JOIN photos p ON p.news_id = n.id
    GROUP BY n.id
  )
  SELECT n.id news_id, n.title news_title, n.date_created news_date_created, 
         n.author_id news_author_id, u.name news_author_name, 
         n.category_id news_category_id, c.name news_category_name, 
         n.content news_content, n.main_photo news_main_photo, n.is_draft news_is_draft, 
         n.photos_num photos_num, t.name news_tag_name
    FROM np n
    JOIN authors a ON n.author_id = a.id
    JOIN users u ON a.user_id = u.id
    JOIN categories c ON n.category_id = c.id
    JOIN tags_news tn ON n.id = tn.news_id
    JOIN tags t ON tn.tag_id = t.id
    WHERE n.is_draft = false  
);