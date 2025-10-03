Таблиці
```
users (id, username, email, password, is_admin)
authors (id, name, faculty) 
software (id, title, author_id, version, annotation, type, usage_terms, image_url)
distributions (id, software_id, path, upload_date)
usage_stats (id, software_id, user_id, date, actions_count)
popularity (id, software_id, user_id, popularity_score)
```