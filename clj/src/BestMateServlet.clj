(ns BestMateServlet
  (:gen-class
   :extends  javax.servlet.http.HttpServlet)
  (:use mate))

(defn -doGet [_ req resp]
  (process-request req resp))