(ns dojo.keys)

(def api-key (or (System/getenv "OPENAIKEY")))

(def telegram-token (or (System/getenv "TELEGRAM_BOT_TOKEN")))
