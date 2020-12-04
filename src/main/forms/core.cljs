(ns forms.core
  (:require [reagent.core :as r]
            [reagent.dom :as rd]
            [re-frame.core :as rf]))

(def _form {:title "The quick brown form"
            :description "A form for you and for me"
            :questions [{:id "q1"
                         :question-text "name?"
                         :type :text
                         :value "John"}
                        {:id "q2"
                         :question-text "age?"
                         :type :text
                         :value "4"}
                        {:id "q3"
                         :question-text "best?"
                         :type :combo
                         :options ["dogs" "cats" "mice" "fish"]
                         :value "cats"}
                        {:id "q4"
                         :question-text "Write me a story.."
                         :type :text-multiline
                         :value "Enter here..."}
                        {:id "q5"
                         :question-text "Which of the following best describes you?"
                         :type :radio
                         :options ["Good" "Bad" "Ugly"]
                         :value "Good"}
                        ]})

(rf/reg-event-db
 :init
 (fn [_ _] {:form _form}))

(defn update-question-data [db id f]
  (update-in db [:form :questions]
             (fn [qs]
               (mapv (fn [q]
                       (if (= (:id q) id)
                         (f q)
                         q))
                     qs))))

(rf/reg-event-db
 :set-question-value
 (fn [db [_ id value]]
   (update-question-data db id #(assoc % :value value))))

(rf/reg-event-fx
 :show-submission
 (fn [cofx _]
   (let [db (:db cofx)
         submission (reduce (fn [s q] (assoc s (:id q) (:value q)))
                            {}
                            (get-in db [:form :questions]))]
     (println submission))))
   
(rf/reg-sub
 :query-form
 (fn [db _] (:form db)))

(defn question-label [question]
  [:<>
   [:label {:for (:id question)} (:question-text question)]
   [:br]])

(defmulti question-component (fn [question] (:type question)))

(defn on-change [q]
  #(rf/dispatch [:set-question-value (:id q) (-> % .-target .-value)]))

(defmethod question-component :text [question]
  [:input {:id (:id question) :type "text" :value (:value question) :on-change (on-change question)}])

(defmethod question-component :text-multiline [question]
  [:textarea {:id (:id question) :cols 80 :rows 24 :value (:value question) :on-change (on-change question)}])

(defmethod question-component :combo [question]
  [:select {:id (:id question) :type "text" :value (:value question) :on-change (on-change question)}
   (for [opt (:options question)]
     [:option {:key opt :value opt} opt])])

(defmethod question-component :radio [question]
  [:<>
   (for [opt (:options question)]
     [:<> {:key opt}
      [:input {:type "radio" :value opt :name (:id question) :checked (= (:value question) opt) :on-change (on-change question)}]
      [:label {:for opt} opt]])])

(defn render-question [question]
  [:div {:key (:id question)}
   [question-label question]
   [question-component question]])

(defn form-view []
  (let [form (rf/subscribe [:query-form])]
    [:div
     [:h1 (:title @form)]
     [:h3 (:description @form)]
     [:form
      (map render-question (:questions @form))]
     [:button {:on-click #(rf/dispatch [:show-submission])} "Submit"]]))

(defn app []
  [form-view])

(defn ^:dev/after-load init []
  (let [root (js/document.getElementById "root")]
    (rf/dispatch [:init])
    (rd/render [app] root)))
