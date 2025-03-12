CREATE TABLE users (
  user_id SERIAL PRIMARY KEY,
  first_name VARCHAR NOT NULL,
  last_name VARCHAR NOT NULL,
  email VARCHAR NOT NULL UNIQUE
);

CREATE TABLE user_notifications (
  user_id INT,
  email_notifications BOOLEAN DEFAULT true,
  push_notifications BOOLEAN DEFAULT true,
  PRIMARY KEY (user_id)
);

CREATE TABLE user_profiles (
  user_id INT PRIMARY KEY,
  bio TEXT,
  age INT,
  github_id VARCHAR
);