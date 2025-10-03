CREATE DATABASE IF NOT EXISTS software_portal
  CHARACTER SET utf8mb4
  COLLATE utf8mb4_unicode_ci;

USE software_portal;

DROP TABLE IF EXISTS popularity;
DROP TABLE IF EXISTS usage_stats;
DROP TABLE IF EXISTS distributions;
DROP TABLE IF EXISTS software;
DROP TABLE IF EXISTS authors;
DROP TABLE IF EXISTS users;

CREATE TABLE IF NOT EXISTS users (
  id INT AUTO_INCREMENT PRIMARY KEY,
  username VARCHAR(50) UNIQUE NOT NULL,
  email VARCHAR(100),
  password VARCHAR(255) NOT NULL,
  is_admin TINYINT(1) NOT NULL DEFAULT 0
);

CREATE TABLE IF NOT EXISTS authors (
  id INT AUTO_INCREMENT PRIMARY KEY,
  name VARCHAR(150) NOT NULL,
  faculty VARCHAR(150)
);

CREATE TABLE IF NOT EXISTS software (
  id INT AUTO_INCREMENT PRIMARY KEY,
  title VARCHAR(200) NOT NULL,
  author_id INT,
  version VARCHAR(50),
  annotation TEXT,
  type VARCHAR(100),
  usage_terms TEXT,
  image_url VARCHAR(300),
  FOREIGN KEY (author_id) REFERENCES authors(id) ON DELETE SET NULL
);

CREATE TABLE IF NOT EXISTS distributions (
  id INT AUTO_INCREMENT PRIMARY KEY,
  software_id INT NOT NULL,
  path VARCHAR(300),
  upload_date DATETIME DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (software_id) REFERENCES software(id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS usage_stats (
  id INT AUTO_INCREMENT PRIMARY KEY,
  software_id INT NOT NULL,
  user_id INT,
  date DATETIME DEFAULT CURRENT_TIMESTAMP,
  actions_count INT NOT NULL DEFAULT 1,
  FOREIGN KEY (software_id) REFERENCES software(id) ON DELETE CASCADE,
  FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE SET NULL
);

CREATE TABLE IF NOT EXISTS popularity (
  id INT AUTO_INCREMENT PRIMARY KEY,
  software_id INT NOT NULL,
  user_id INT,
  popularity_score INT NOT NULL,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  UNIQUE KEY uniq_popularity (software_id, user_id),
  FOREIGN KEY (software_id) REFERENCES software(id) ON DELETE CASCADE,
  FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE SET NULL
);

-- Seed admin and sample data
INSERT INTO users (username, email, password, is_admin)
VALUES ('admin', 'admin@example.com', 'admin123', 1)
ON DUPLICATE KEY UPDATE email=VALUES(email), password=VALUES(password), is_admin=VALUES(is_admin);

INSERT INTO authors (name, faculty)
VALUES ('Іван Петренко', 'Факультет комп’ютерних наук')
ON DUPLICATE KEY UPDATE faculty=VALUES(faculty);

INSERT INTO software (title, author_id, version, annotation, type, usage_terms, image_url)
VALUES ('Навчальний портал', 1, '1.0.0', 'Система для курсових матеріалів', 'web', 'Використання лише в навчальних цілях', NULL)
ON DUPLICATE KEY UPDATE version=VALUES(version), annotation=VALUES(annotation), type=VALUES(type), usage_terms=VALUES(usage_terms), image_url=VALUES(image_url);

INSERT INTO distributions (software_id, path)
VALUES (1, '/srv/software/portal/dist-1.0.0.zip');
