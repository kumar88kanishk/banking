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

(defn transactions [account]
  (:transactions account))

(defn last-transaction [transactions]
  (or (last transactions) {}))

(defn last-balance [transaction]
  (or (:balance transaction) 0))

(defn credit [account amount]
  (let [last-balance (last-balance account)]
    (+ last-balance amount)))

(defn debit [account amount]
  (let [last-balance (last-balance account)]
    (- last-balance amount)))


;(debit-in-current {:password 123 :username "kansihk" :accounts [{:type "saving" :transactions []}], :account-number "123"} 23)
(defn merge-transactions [transaction new-transaction]
  (conj transaction new-transaction))

(defn new-transaction [amount type balance]
  (hash-map :type type :balance balance :amount amount))

(defn refresh-account [account new-transactions]
  (assoc account :transactions new-transactions))

(defn transact [inputs action]
  (let [valid-account (is-valid-account inputs)]
    (case (:action action)
      "credit-in-saving" (do
                           (let [new-balance (credit (last-transaction (saving-account valid-account)) (:amount action))]
                             (let [new-transaction (new-transaction (:amount action) "credit" new-balance)]
                               (let [merge-transactions (merge-transactions (transactions (saving-account valid-account)) new-transaction)]
                                 (let [updated-account (refresh-account (saving-account valid-account) merge-transactions)]
                                   updated-account)))))
      "debit-in-saving" (debit  (last-transaction (saving-account valid-account)) (:amount action))
      "credit-in-current" (credit (last-transaction (current-account valid-account)) (:amount action))
      "debit-in-current" (debit (last-transaction (current-account valid-account)) (:amount action)))))


(defn login [inputs type]
  (let [valid-account (is-valid-account inputs)]
    valid-account))

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

(conj {:a "b" :c [{:a "b"}]} {:c [{:b "x"}]})
(assoc [1 2 3] 1 0)

(.indexOf (db) {:account-number "07435",
                :password "123",
                :username "kanishk",
                :accounts [{:type "saving", :transactions [{:type "credit", :amount 123, :balance 123}]}],
                })