(ns b.core
  (:require [clojure.data.json :as json]))


(defn db [] (json/read-str (slurp "resources/users.json") :key-fn keyword))

(defn is-valid-account [input]
  (first
    (let [given-keys (keys input)]
      (for [account (db)
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

(defn credit [account amount]
  (let [last-balance (last-balance account)]
    (+ last-balance amount)))

(defn debit [account amount]
  (let [last-balance (last-balance account)]
    (- last-balance amount)))


;(debit-in-current {:password 123 :username "kansihk" :accounts [{:type "saving" :transactions []}], :account-number "123"} 23)
(defn merge-transactions [transaction new-balance old-balance]
  (conj transaction (hash-map :new-balance new-balance :old-balance old-balance)))

(defn refresh-account [account new-transactions]
  (conj (:accounts account) new-transactions))

(defn transact [inputs type]
  (let [valid-account (is-valid-account inputs)]
    (print valid-account)
    (case (:action type)
      "credit-in-saving" (do
                           (let [new-balance (credit (saving-account-last-transaction valid-account) (:amount type))]
                             (let [new-transaction (merge-transactions (saving-account-transactions valid-account) new-balance (last-balance valid-account))]
                               (refresh-account (saving-account-transactions valid-account) new-transaction))))
      "debit-in-saving" (debit  (saving-account-last-transaction valid-account ) (:amount type))
      "credit-in-current" (credit (current-account-last-transaction valid-account ) (:amount type))
      "debit-in-current" (debit (current-account-last-transaction valid-account ) (:amount type)))))


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
(transact {:username "kanishk" :password "123"} {:action "credit-in-saving" :amount 12})

(conj {:a "b"} {:a "c"})