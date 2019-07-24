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
      (assoc (db) (.indexOf (db) user) (merge user (:accounts (assoc (:accounts user) account-index updated-account)))))))

(defn write-in-db [data]
  (print data "aaasdasdads")
  (spit "src/accounts.json" (json/write-str data)))

(defn transact [inputs action]
  (let [valid-user (is-valid-account inputs)]
    (case (:action action)
      "credit-in-saving" (do
                           (let [new-balance (credit (last-transaction (transactions (saving-account valid-user))) (:amount action))]
                             (let [new-transaction (new-transaction (:amount action) "credit" new-balance)]
                               (let [merge-transactions (merge-transactions (transactions (saving-account valid-user)) new-transaction)]
                                 (let [updated-user-account (refresh-account valid-user (saving-account valid-user) merge-transactions)]
                                   (write-in-db updated-user-account))))))
      "debit-in-saving" (debit  (last-transaction (saving-account valid-user)) (:amount action))
      "credit-in-current" (credit (last-transaction (current-account valid-user)) (:amount action))
      "debit-in-current" (debit (last-transaction (current-account valid-user)) (:amount action)))))


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
  (spit "src/accounts.json" (json/write-str (prepare-registration-attributes account))))

(login {:username "kanishk" :password "123"})
(register-account {:username "kanishk" :password "123"})
(transact {:username "kanishk" :password "123"} {:action "credit-in-saving" :amount 12})

(conj {:a "b" :c [{:a "b"}]} {:c [{:b "x"}]})
(assoc ["a" "b" "c"] 0 2)

(.indexOf (db) {:account-number "07435",
                :password "123",
                :username "kanishk",
                :accounts [{:type "saving", :transactions [{:type "credit", :amount 123, :balance 123}]}],
                })