(ns b.core
  (:require [clojure.data.json :as json]))


(def db (json/read-str (slurp "src/accounts.json") :key-fn keyword))

(defn is-valid-account [input]
  (first
    (let [given-keys (keys input)]
      (for [account db
            :let [found (select-keys account given-keys)]
            :when (= input found)]
        account))))


(defn saving-account [account]
  (first (filter #(= "saving" (:type %))
                 (:accounts account))))

(defn current-account [account]
  (first (filter #(= "current" (:type %))
                 (:accounts account))))

(defn saving-account-transactions [account]
  (:transactions (saving-account account)))

(defn current-account-transactions [account]
  (:transactions (current-account account)))

(defn saving-account-last-transaction [account]
  (or (last (saving-account-transactions account)) {}))

(defn current-account-last-transaction [account]
  (or (last (current-account-transactions account)) {}))

(defn last-balance [transaction]
  (or (:balance transaction) 0))

(defn credit-in-saving [account amount]
  (let [last-balance (last-balance (saving-account-last-transaction account))]
    (+ last-balance amount)))

(defn debit-in-saving [account amount]
  (let [last-balance (last-balance (saving-account-last-transaction account))]
    (- last-balance amount)))

(defn credit-in-current [account amount]
  (let [last-balance (last-balance (current-account-last-transaction account))]
    (+ last-balance amount)))

(defn debit-in-current [account amount]
  (let [last-balance (last-balance (current-account-last-transaction account))]
    (- last-balance amount)))

;(debit-in-current {:password 123 :username "kansihk" :accounts [{:type "saving" :transactions []}], :account-number "123"} 23)

(defn transact [inputs type]
  (let [valid-account (is-valid-account inputs)]
    (case (:action type)
      "credit-in-saving" (credit-in-saving valid-account (:amount type))
      "debit-in-saving" (debit-in-saving valid-account (:amount type))
      "credit-in-current" (credit-in-current valid-account (:amount type))
      "debit-in-current" (debit-in-current valid-account (:amount type)))))
(defn login [inputs type]
  (let [valid-account (is-valid-account inputs)]
    (case (:account-type type)
      "saving" (saving-account-transactions valid-account)
      "current" (current-account-transactions valid-account))))

(defn prepare-registration-attributes [account]
  (let [account-number (reduce str (set (take 6 (repeatedly #(rand-int 9)))))]
          (hash-map :account-number account-number
                    :username (:username account)
                    :password (:password account)
                    :accounts (vector (hash-map :transactions [] :type "saving"))
                    )))
(defn register-account [account]
  (spit "src/accounts.json" (json/write-str (prepare-registration-attributes account))))

(login {:username "kanishk" :password "123"} {:account-type "saving"})
(register-account {:username "kanishk" :password "123"})
(transact {:username "kanishk" :password "123"} {:action "debit-in-saving" :amount 12})

