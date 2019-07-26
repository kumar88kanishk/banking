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

(defn saving-account [user]
  (first (filter #(= "saving" (:type %))
                 (:accounts user))))

(defn current-account [user]
  (first (filter #(= "current" (:type %))
                 (:accounts user))))

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

(defn merge-transactions [transaction new-transaction]
  (conj transaction new-transaction))

(defn new-transaction [amount type balance]
  (hash-map :type type :balance balance :amount amount))

(defn refresh-account [ user account new-transactions]
  (let [updated-account (assoc account :transactions new-transactions)]
    (let [account-index (.indexOf (:accounts user) account)]
      (assoc (db) (.indexOf (db) user) (merge user (hash-map :accounts (assoc (:accounts user) account-index updated-account)))))))

(defn write-in-db [data]
  (print "write data" data)
  (spit "resources/users.json" (json/write-str data)))

(defn credit-with-last-balance [account amount]
  (credit (last-transaction (transactions account)) amount))

(defn debit-with-last-balance [account amount]
  (debit (last-transaction (transactions account)) amount))

(defn calculate-transaction [valid-user new-balance action amount]
  (let [new-transaction (new-transaction amount action new-balance)]
    (let [merge-transactions (merge-transactions (transactions (saving-account valid-user)) new-transaction)]
      (let [updated-user-account (refresh-account valid-user (saving-account valid-user) merge-transactions)]
        (write-in-db updated-user-account)))))

(defn transact [inputs params]
  (let [valid-user (is-valid-account inputs)]
    (case (:action params)
      "credit-in-saving" (calculate-transaction valid-user (credit-with-last-balance (saving-account valid-user) (:amount params)) "credit" (:amount params))
      "debit-in-saving" (calculate-transaction valid-user (debit-with-last-balance (saving-account valid-user) (:amount params)) "debit" (:amount params))
      "credit-in-current" (calculate-transaction valid-user (credit-with-last-balance (current-account valid-user) (:amount params)) "credit" (:amount params))
      "debit-in-current" (calculate-transaction valid-user (debit-with-last-balance (current-account valid-user) (:amount params)) "debit" (:amount params)))))


(defn login [inputs]
  (let [valid-user (is-valid-account inputs)]
    valid-user))

(defn prepare-registration-attributes [account]
  (let [account-number (reduce str (set (take 6 (repeatedly #(rand-int 9)))))]
    (hash-map :account-number account-number
              :username (:username account)
              :password (:password account)
              :accounts (vector (hash-map :transactions [] :type "saving"))
              )))

(defn register-account [account]
  (spit "resources/users.json" (json/write-str (prepare-registration-attributes account))))

(login {:username "kanishk" :password "123"})
(register-account {:username "kanishk" :password "123"})
(transact {:username "kanishk" :password "123"} {:action "debit-in-saving" :amount 20})

(conj {:a "b" :c [{:a "b"}]} {:c [{:b "x"}]})
(assoc ["a" "b" "c"] 0 2)

(.indexOf (db) {:account-number "07435",
                :password "123",
                :username "kanishk",
                :accounts [{:type "saving", :transactions [{:type "credit", :amount 123, :balance 123}]}],
                })