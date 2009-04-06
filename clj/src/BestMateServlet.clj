(ns BestMateServlet
  (:gen-class
   :extends  javax.servlet.http.HttpServlet)
  (:import
   [javax.servlet.http HttpServlet]
   [javax.servlet.http HttpServletRequest]
   [javax.servlet.http HttpServletResponse])
  (:use mate))


(defn- write-to-resp [resp text]
  (. (. resp getWriter) println text))

(defn -doGet [_ req resp]
  (write-to-resp resp "WTF?"))