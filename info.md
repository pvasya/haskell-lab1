Scotty + mysql-simple



Таблиці
```
users (id, username, email, password_hash)
authors (id, name, faculty) 
software (id, title, author_id, version, annotation, type, usage_terms, image_url)
distributions (id, software_id, path, upload_date)
usage_stats (id, software_id, user_id, date, actions_count)
popularity (id, software_id, popularity_score, votes)
```


Routes
```
GET / — показати список програм (назва, автор, версія, рейтинг).
GET /software/:id — показати деталі (анотація, версія, дистрибутиви, статистика, популярність).
GET /register — форма реєстрації.
POST /register — реєстрація користувача.
GET /login — форма входу.
POST /login — перевірка користувача і створення сесії.
GET /logout — вихід із системи.
GET /add — форма для додавання нового ПЗ (тільки авторизований користувач).
POST /add — додати нову програму в базу.
GET /edit/:id — форма редагування даних про ПЗ (тільки авторизований користувач).
POST /edit/:id — оновити дані про ПЗ.
POST /delete/:id — видалити програму.
POST /vote/:id — проголосувати за популярність ПЗ (оновлює таблицю popularity).
```


SQL
```
CREATE DATABASE software_portal
  DEFAULT CHARACTER SET utf8mb4
  DEFAULT COLLATE utf8mb4_unicode_ci;

USE software_portal;

CREATE TABLE users (
    id INT AUTO_INCREMENT PRIMARY KEY,
    username VARCHAR(100) NOT NULL UNIQUE,
    email VARCHAR(150) NOT NULL UNIQUE,
    password_hash VARCHAR(255) NOT NULL
);

CREATE TABLE authors (
    id INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(200) NOT NULL,
    faculty VARCHAR(200)
);

CREATE TABLE software (
    id INT AUTO_INCREMENT PRIMARY KEY,
    title VARCHAR(200) NOT NULL,
    author_id INT,
    version VARCHAR(50),
    annotation TEXT,
    type VARCHAR(100),
    usage_terms VARCHAR(255),
    image_url VARCHAR(255),
    FOREIGN KEY (author_id) REFERENCES authors(id) ON DELETE SET NULL
);

CREATE TABLE distributions (
    id INT AUTO_INCREMENT PRIMARY KEY,
    software_id INT NOT NULL,
    path VARCHAR(255) NOT NULL,
    upload_date DATE,
    FOREIGN KEY (software_id) REFERENCES software(id) ON DELETE CASCADE
);

CREATE TABLE usage_stats (
    id INT AUTO_INCREMENT PRIMARY KEY,
    software_id INT NOT NULL,
    user_id INT NOT NULL,
    date DATE NOT NULL,
    actions_count INT DEFAULT 0,
    FOREIGN KEY (software_id) REFERENCES software(id) ON DELETE CASCADE,
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
);

CREATE TABLE popularity (
    id INT AUTO_INCREMENT PRIMARY KEY,
    software_id INT NOT NULL,
    popularity_score INT DEFAULT 0,
    votes INT DEFAULT 0,
    FOREIGN KEY (software_id) REFERENCES software(id) ON DELETE CASCADE
);
```
