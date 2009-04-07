(ns mate
  (:import
   (java.net URL)
   (java.io InputStreamReader)
   (java.io BufferedReader)
   (org.apache.commons.logging Log)
   (org.apache.commons.logging LogFactory))

  (:use [clojure.xml :only (parse)]
	[clojure.contrib.str-utils :only (re-partition)]))

(defn- log-info [ & txt]
  (. (. LogFactory getLog (class " ")) info (str txt)))

(defn- log-error [txt exception]
  (. (. LogFactory getLog (class " ")) error txt exception))

(defn- GET-body [uri]
  (with-open [reader (BufferedReader. (InputStreamReader. (. (URL. uri) openStream)))]
    (apply str (line-seq reader))))

(defn- write-to-resp [resp text]
  (. (. resp getWriter) println text))

(defn- find-username [uri]
  (first (re-seq #"\w+$" uri)))

(defn- twitter-page-url [username]
  (str "http://twitter.com/" username))

(defn- twitter-page-for [username]
  (let [uri (twitter-page-url username)]
    (log-info uri)
    (GET-body uri)))

(defn- atom-url-in [html-page]
  (first 
   (re-seq #"http://twitter.com/statuses/user_timeline/\w+.atom" 
	   html-page)))

(defn- atom-entries-in [atom-feed]
  (map #(first (:content (first %)))
       (map :content 
	    (filter 
	     #(= :entry (:tag %)) 
	     (:content atom-feed)))))

(defn- twits-in [atom-feed]
  (map 
   #(second (rest (re-partition #"^\w+: " %)))
   (atom-entries-in atom-feed)))


(defn- people-mentionated-in [twit]
  (re-seq #"@\w+" twit))

(defn- replied-friends-in [twit-list]
  (reduce
   (fn[acc l] (into acc (people-mentionated-in l)))
   []
   twit-list))

(defn- number-times-replied [replied-friend-list]
  (reduce 
   (fn [acc f] (merge-with + acc {f 1}))
   {}
   replied-friend-list))

(defn- error-in-feed [feed]
  (:content (next (:content feed))))

(defn- is-error-message? [feed]
  (some #(= % :error) (map :tag feed)))

(defn- twitter-feed-for [username]
  (let [uri (atom-url-in (twitter-page-for username))]
    (if uri
      (do
	(log-info "getting " uri)
	(let [result (parse uri)]
	  (if (is-error-message? result)
	    (throw error-in-feed result)
	    result))))))
    
(defn- most-replied [replies-map]
  (first (reduce 
	  (fn [m f] (if (> (second f) (second m))
		      f
		      m))
	  replies-map)))

(defn- remove-at-sign [login]
  (first (re-seq #"\w+$" login)))

(defn process-request [req resp]
  (write-to-resp resp "<html><head><title>Your Best (twitter) Mate!</title></head><body>")
  (try
   (let [username (find-username (. req getRequestURI))]
     (if username
       (let[twits (twits-in (twitter-feed-for username))]
	 (if (seq twits)
	   (let[friends (replied-friends-in twits)
		best-mate (most-replied (number-times-replied friends))]
	     (write-to-resp resp (str "Your best mate is: <a href=\"" (twitter-page-url (remove-at-sign best-mate))  "\">" best-mate "</a>")))
	   (write-to-resp resp (str "Could not find twits for user <a href=\"" (twitter-page-url username)  "\">" username "</a>"))))
       (write-to-resp resp "Could not find twitter user in URI")))
     (catch Exception e
       (log-error "Problem with request" e)
       (write-to-resp resp (. e getMessage))))
  (write-to-resp resp "</body></html>"))