(ns quant.advisor
  "QuantitativeProfessionalsAdvisor — proposes a statistical-analysis
  operation (approve an analysis, publish a finding) for a registered
  organization. Swappable mock/llm; the advisor ONLY proposes —
  `quant.governor` checks the significance arithmetic and method
  membership independently. Modeled on cloud-itonami-isco-4311's
  advisor.

  A proposal: {:op :approve-analysis|:publish-finding
               :effect :propose :model-id str :p-value number
               :method str :claim kw :stake kw :confidence n
               :rationale str}")

(defprotocol Advisor
  (-advise [advisor store request] "request -> proposal map"))

(defn- infer [_store {:keys [op stake model-id p-value method claim] :as request}]
  {:op op
   :effect :propose
   :model-id model-id
   :p-value p-value
   :method method
   :claim claim
   :stake (or stake :low)
   :confidence (case (or stake :low) :high 0.7 :medium 0.85 :low 0.95)
   :rationale (str "proposed " (name op) " for client " (:client-id request))})

(defn mock-advisor []
  (reify Advisor
    (-advise [_ store request] (infer store request))))

(def ^:private system-prompt
  "You are a statistical/actuarial advisor. Given a request, propose
   an :op, the :model-id, :p-value, :method and :claim, an honest
   :confidence and a :stake. Never call a significance claim
   conforming when the p-value exceeds the registered threshold, and
   never use a method outside the registered approved set — the
   governor checks both.")

(defn- parse-proposal [content]
  (try
    (let [p (read-string content)]
      (if (map? p)
        (assoc p :effect :propose)
        {:op :unknown :effect :propose :confidence 0.0 :stake :high
         :rationale "unparseable LLM response"}))
    (catch #?(:clj Exception :cljs js/Error) _
      {:op :unknown :effect :propose :confidence 0.0 :stake :high
       :rationale "LLM response parse failure"})))

(defn llm-advisor
  [chat-model model-generate-fn gen-opts]
  (reify Advisor
    (-advise [_ _store request]
      (let [msgs [{:role :system :content system-prompt}
                  {:role :user :content (str "operation request: " (pr-str request))}]
            resp (model-generate-fn chat-model msgs gen-opts)]
        (parse-proposal (:content resp))))))
