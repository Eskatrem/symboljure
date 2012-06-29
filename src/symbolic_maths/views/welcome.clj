(ns symbolic-maths.views.welcome
  (:require [symbolic_maths.views.common :as common]
            [noir.content.pages :as pages]
	    [noir.response :as resp]
	    [noir.validation :as vali])
  (:use noir.core
        hiccup.core
        hiccup.page-helpers
	hiccup.form-helpers
	clojure.contrib.str-utils
	noir.core))

(defpage "/welcome" []
         (common/layout
	  [:p "Welcome to test-noir"]))

(defpage "/test" []
  (common/layout
   [:p "just testing"])
  (html [:p "test"]))

(defpage [:post "/login"] {:keys [username password]}
  (println "username = " username)
  (str "you tried to login as " username " with " password))

(defpage [:get "/"] [] "This is a get") ;; same as (defpage "/" [] ..)

;;(defpage "/user/:id" {:keys [id]}
  ;;(str "You are user number " id))

(defpage "/test-input" []
  (html (form-to [:post "/login"]
               (text-field "Username")
               (password-field "Password")
               (submit-button "Login"))))

(defpage "/test-input2" []
  (html (form-to [:post "/my-get"]
		 (text-field "test-input: " :test-input)
		 (submit-button "enter"))))

(defpage "/my-get"  {:keys [test-input]}
  (str "test-input = " test-input))

(defpartial layout [& content]
  (html5
    [:head
     [:title "Forms"]]
    [:body
     content]))

(defpartial user-fields [{:keys [firstname lastname]}]
  (label "firstname" "First name: ")
  (text-field "firstname" firstname)
  (label "lastname" "Last name: ")
  (text-field "lastname" lastname))

;; (defpage ["/user/add"] {:as user}
;;   (layout
;;     (form-to [:post "/user/add"]
;;             (user-fields user)
;;             (submit-button "Add user"))))

(defn valid? [{:keys [firstname lastname]}]
  (vali/rule (vali/min-length? firstname 5)
             [:firstname "Your first name must have more than 5 letters."])
  (vali/rule (vali/has-value? lastname)
             [:lastname "You must have a last name"])
  (not (vali/errors? :lastname :firstname)))

;; (defpage [:post "/user/add"] {:as user}
;;   (if (valid? user)
;;     (do (println (reverse (:firstname user)))
;; 	(layout
;;      [:p (str "User " (:firstname user) " added!")]))
;; 	;;(str (:firstname user))
;; 	;;(println user))
;;     (render "/user/add" user)))

(defpage [:post "/test-post"] {:as input}
  )

(defpartial add-user-form [{:keys [firstname lastname] :as user}]
 (form-to [:post "/user/add"]
   (label "firstname" "First name: ")
   (text-field "firstname" firstname)
   (label "lastname" "Last name: ")
   (text-field "lastname" lastname)
   (submit-button "Add user")))

(defpage [:get "/user/add"] {:as user}
 (layout
   [:div (add-user-form user)]))

(defpage [:post "/user/add"] {:as user}
 (layout
   [:p (str "User " (str-join (reverse (:firstname user)) "") " added!")]
   [:div (add-user-form nil)]))

(defpartial get-formula-form [formula]
  nil)

(defpage [:get "/maths"] {:as formula}
  (layout
   [:div (get-formula-form formula)]))

(defpage [:post "/maths"] {:as formula}
  (layout ""))