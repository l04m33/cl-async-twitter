(in-package #:cl-async-twitter)


(defvar *resource-base-url* "https://api.twitter.com/1.1/"
  "Base URL for accessing Twitter API resources.")
(defvar *request-token-url* "https://api.twitter.com/oauth/request_token"
  "Twitter OAuth request token URL.")
(defvar *authenticate-url* "https://api.twitter.com/oauth/authenticate"
  "Twitter OAuth authentication URL.")
(defvar *access-token-url* "https://api.twitter.com/oauth/access_token"
  "Twitter OAuth access token URL.")
(defvar *user-stream-url* "https://userstream.twitter.com/1.1/user.json"
  "Twitter user stream API.")
(defvar *friendships-create-url* "https://api.twitter.com/1.1/friendships/create.json"
  "Twitter API for following other users.")
(defvar *direct-messages-new-url* "https://api.twitter.com/1.1/direct_messages/new.json"
  "Twitter API for sending direct messages.")
(defvar *statuses-user-timeline-url* "https://api.twitter.com/1.1/statuses/user_timeline.json"
  "Twitter user timeline API.")
(defvar *statuses-update-url* "https://api.twitter.com/1.1/statuses/update.json"
  "Twitter tweeting API.")
(defvar *users-lookup-url* "https://api.twitter.com/1.1/users/lookup.json"
  "Twitter user lookup API.")
