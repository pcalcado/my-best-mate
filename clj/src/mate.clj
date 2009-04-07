(ns mate
  (:import
   (java.net URL)
   (java.io InputStreamReader)
   (java.io BufferedReader))
  (:use [clojure.xml :only (parse)]
	[clojure.contrib.str-utils :only (re-partition)]))

(defn- GET-body [uri]
  (with-open [reader (BufferedReader. (InputStreamReader. (. (URL. uri) openStream)))]
    (apply str (line-seq reader))))

(defn- write-to-resp [resp text]
  (. (. resp getWriter) println text))

(defn- find-username [uri]
  (first (re-seq #"\w+$" uri)))

(defn- twitter-page-for [username]
  (let [uri (str "http://twitter.com/" username)]
    (println uri)
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

(defn- twitter-feed-for [username]
  (let [uri (atom-url-in (twitter-page-for username))]
    (if uri
      (do
	(println "getting " uri)
	(parse uri)))))
    
(defn- most-replied [replies-map]
  (first (reduce 
	  (fn [m f] (if (> (second f) (second m))
		      f
		      m))
	  replies-map)))

(defn process-request [req resp]
  (let [username (find-username (. req getRequestURI))]
    (if username
      (let[twits (twits-in (twitter-feed-for username))]
	(if twits
	  (let[friends (replied-friends-in twits)
	       best-mate (most-replied (number-times-replied friends))]
	    (write-to-resp resp (str "Your best mate is: " best-mate))))))))