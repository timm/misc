;;; core.fnl: shared structures, distance, stats, search core.
;;; (c) 2026 Tim Menzies, timm@ieee.org, MIT license
;;;
;;; Shared options (used in >1 app):
;;;     --seed=1             random number seed
;;;     --p=2                Minkowski exponent
;;;     --few=128            sample cap for cluster/acquire
;;;     --show.show=30       tree display width
;;;     --show.decimals=2    decimal places
;;;     --stats.cliffs=0.195 Cliffs Delta threshold
;;;     --stats.conf=1.36    KS test confidence
;;;     --stats.eps=0.35     margin of error multiplier
;;;     --bayes.m=2          m-estimate for Naive Bayes
;;;     --bayes.k=1          k-estimate (Laplace) for NB

;; -- aliases --------------------------------------------------------
(local sqrt  math.sqrt)
(local exp   math.exp)
(local pi    math.pi)
(local huge  math.huge)
(local floor math.floor)
(local abs   math.abs)
(local fmt   string.format)

(fn log  [x] (math.log x))
(fn log2 [x] (math.log x 2))
(fn rand [] (math.random))
(fn srand [s] (math.randomseed (or s 1)))

(local the {})

;; -- tiny utils on lists --------------------------------------------
(fn map [t f]
  (let [out []]
    (each [_ v (ipairs t)] (table.insert out (f v)))
    out))

(fn keep [t f]
  (let [out []]
    (each [_ v (ipairs t)] (when (f v) (table.insert out v)))
    out))

(fn sum [t]
  (var s 0)
  (each [_ v (ipairs t)] (set s (+ s v)))
  s)

(fn copy [t]
  (let [out []] (each [_ v (ipairs t)] (table.insert out v)) out))

(fn slice [t ?lo ?hi]
  (let [lo (or ?lo 1) hi (or ?hi (length t)) out []]
    (for [i lo hi] (table.insert out (. t i)))
    out))

(fn min-by [t f]
  (var best nil) (var bv huge)
  (each [_ x (ipairs t)]
    (let [v (f x)] (when (< v bv) (set best x) (set bv v))))
  best)

(fn max-by [t f]
  (var best nil) (var bv (- huge))
  (each [_ x (ipairs t)]
    (let [v (f x)] (when (> v bv) (set best x) (set bv v))))
  best)

(fn shuffle [t]
  (for [i (length t) 2 -1]
    (let [j (math.random i) tmp (. t i)]
      (tset t i (. t j)) (tset t j tmp)))
  t)

(fn sample [t ?n]
  (slice (shuffle (copy t)) 1 (or ?n (length t))))

(fn choice [t] (. t (math.random (length t))))

(fn choices [t k]
  (let [out [] n (length t)]
    (for [_ 1 k] (table.insert out (. t (math.random n))))
    out))

;; -- tagged-table constructors --------------------------------------
(fn last-char [s]
  (if (= s "") "" (string.sub s -1)))

(fn upper? [c]
  (let [b (string.byte c)]
    (and b (>= b 65) (<= b 90))))

(fn Num [?txt ?at]
  {:type :Num
   :txt (or ?txt "") :at (or ?at 0)
   :n 0 :mu 0 :m2 0 :sd 0
   :heaven (not= "-" (last-char (or ?txt "")))})

(fn Sym [?txt ?at]
  {:type :Sym :txt (or ?txt "") :at (or ?at 0) :n 0 :has {}})

(fn Col [?txt ?at]
  (let [t (or ?txt "") f (string.sub t 1 1)]
    ((if (upper? f) Num Sym) t ?at)))

;; -- column stats ---------------------------------------------------
(fn mode [d]
  (var bk nil) (var bn (- huge))
  (each [k v (pairs d)] (when (> v bn) (set bk k) (set bn v)))
  bk)

(fn entropy [d]
  (var n 0)
  (each [_ v (pairs d)] (set n (+ n v)))
  (var h 0)
  (each [_ v (pairs d)]
    (let [p (/ v n)] (set h (- h (* p (log2 p))))))
  h)

(fn mid    [c] (case c.type :Num c.mu :Sym (mode c.has)))
(fn spread [c] (case c.type :Num c.sd :Sym (entropy c.has)))

(fn norm [num v]
  (if (= v "?") v
      (let [z0 (/ (- v num.mu) (+ num.sd 1e-32))
            z  (math.max -3 (math.min 3 z0))]
        (/ 1 (+ 1 (exp (* -1.7 z)))))))

(fn list-remove [t v]
  (var idx 0)
  (each [i x (ipairs t)] (when (and (= idx 0) (= x v)) (set idx i)))
  (when (> idx 0) (table.remove t idx)))

;; -- add (data/cols/sym/num dispatch) -------------------------------
(fn add [it v ?w]
  (let [w (or ?w 1)]
    (case it.type
      :Data (do (set it._centroid nil)
                (add it.cols v w)
                (if (> w 0) (table.insert it.rows v)
                    (list-remove it.rows v)))
      :Cols (each [_ col (ipairs it.all)]
              (add col (. v col.at) w))
      :Sym  (when (not= v "?")
              (tset it.has v (+ w (or (. it.has v) 0))))
      :Num  (when (not= v "?")
              (if (and (< w 0) (<= it.n 2))
                  (do (set it.n 0) (set it.mu 0)
                      (set it.m2 0) (set it.sd 0))
                  (let [_ (set it.n (+ it.n w))
                        delta (- v it.mu)]
                    (set it.mu (+ it.mu (/ (* w delta) it.n)))
                    (set it.m2 (+ it.m2 (* w delta (- v it.mu))))
                    (set it.sd (if (> it.n 1)
                                   (sqrt (/ (math.max 0 it.m2) (- it.n 1)))
                                   0)))))))
  v)

(fn sub [it v] (add it v -1))

;; -- Cols & Data ----------------------------------------------------
(fn Cols [names]
  (let [c {:type :Cols :names names :klass nil :xs [] :ys [] :all []}]
    (each [j txt (ipairs names)]
      (let [col (Col txt j) tail (last-char txt)]
        (table.insert c.all col)
        (when (not= tail "X")
          (when (= tail "!") (set c.klass col))
          (table.insert
            (if (or (= tail "+") (= tail "-") (= tail "!")) c.ys c.xs)
            col))))
    c))

(fn Data [?src]
  (let [src (or ?src [])
        d   {:type :Data :rows [] :_centroid nil :cols nil}]
    (when (> (length src) 0)
      (set d.cols (Cols (. src 1)))
      (for [i 2 (length src)] (add d (. src i))))
    d))

(fn clone [data ?rows]
  (let [d (Data [data.cols.names])]
    (when ?rows (each [_ r (ipairs ?rows)] (add d r)))
    d))

(fn adds [src ?it]
  (let [it (or ?it (Num))]
    (each [_ v (ipairs (or src []))] (add it v))
    it))

(fn mids [data]
  (when (not data._centroid)
    (set data._centroid (map data.cols.all mid)))
  data._centroid)

;; -- Distance -------------------------------------------------------
(fn minkowski [items p]
  (var tot 0) (var n 1e-32)
  (each [_ v (ipairs items)]
    (set tot (+ tot (^ v p)))
    (set n (+ n 1)))
  (^ (/ tot n) (/ 1 p)))

(fn aha [col u v]
  (if (and (= u "?") (= v "?")) 1
      (= col.type :Sym) (if (= u v) 0 1)
      (let [nu0 (norm col u)
            nv0 (norm col v)
            nu  (if (= nu0 "?") (if (> nv0 0.5) 0 1) nu0)
            nv  (if (= nv0 "?") (if (> nu0 0.5) 0 1) nv0)]
        (abs (- nu nv)))))

(fn disty [data row]
  (let [vals []]
    (each [_ y (ipairs data.cols.ys)]
      (let [nv (norm y (. row y.at)) h (if y.heaven 1 0)]
        (when (not= nv "?") (table.insert vals (abs (- nv h))))))
    (minkowski vals the.p)))

(fn distx [data r1 r2]
  (let [vals []]
    (each [_ x (ipairs data.cols.xs)]
      (table.insert vals (aha x (. r1 x.at) (. r2 x.at))))
    (minkowski vals the.p)))

(fn nearest [data row ?rows]
  (min-by (or ?rows data.rows) #(distx data row $)))

(fn wins [data]
  (let [ys (map data.rows #(disty data $))]
    (table.sort ys)
    (let [ten (floor (/ (length ys) 10))
          lo  (. ys 1)
          med (. ys (+ (* 5 ten) 1))
          sd  (/ (- (. ys (+ (* 9 ten) 1)) (. ys (+ ten 1))) 2.56)]
      (fn [row]
        (var x (disty data row))
        (when (< x (+ lo (* 0.35 sd))) (set x lo))
        (math.max -100 (floor (* 100 (- 1 (/ (- x lo) (+ (- med lo) 1e-32))))))))))

;; -- Bayes ----------------------------------------------------------
(fn like [col v prior]
  (case col.type
    :Sym (/ (+ (or (. col.has v) 0) (* the.bayes.k prior))
            (+ col.n the.bayes.k))
    :Num (let [sd (+ col.sd 1e-32) z (* 2 sd sd)]
           (/ (exp (/ (- (^ (- v col.mu) 2)) z)) (sqrt (* pi z))))))

(fn likes [data row n-rows n-klasses]
  (let [prior (/ (+ (length data.rows) the.bayes.m)
                 (+ n-rows (* the.bayes.m n-klasses)))]
    (var s (log prior))
    (each [_ col (ipairs data.cols.xs)]
      (let [v (. row col.at)]
        (when (not= v "?")
          (let [x (like col v prior)]
            (when (> x 0) (set s (+ s (log x))))))))
    s))

;; -- Sampling -------------------------------------------------------
(fn pick [it ?v]
  (case it.type
    :Sym (pick it.has)
    :Num (let [tmp (if (and ?v (not= ?v "?")) ?v it.mu)
               lo  (- it.mu (* 3 it.sd))
               hi  (+ it.mu (* 3 it.sd))
               new (+ tmp (* it.sd 2 (- (+ (rand) (rand) (rand)) 1.5)))]
           (+ lo (% (- new lo) (+ (- hi lo) 1e-32))))
    _ (do (var n 0)
          (each [_ v (pairs it)] (set n (+ n v)))
          (set n (* n (rand)))
          (var k nil)
          (each [kk v (pairs it)]
            (when (= k nil)
              (set n (- n v))
              (when (<= n 0) (set k kk))))
          k)))

(fn picks [data row ?n]
  (let [n (or ?n 1)
        s (copy row)
        cs (sample data.cols.xs (math.min n (length data.cols.xs)))]
    (each [_ col (ipairs cs)]
      (tset s col.at (pick col (. s col.at))))
    s))

;; -- Utilities ------------------------------------------------------
(fn dinc [k1 k2 ?b4]
  (let [b4 (or ?b4 {})]
    (when (not (. b4 k1)) (tset b4 k1 {}))
    (let [inner (. b4 k1)]
      (tset inner k2 (+ 1 (or (. inner k2) 0))))
    b4))

(fn thing [s]
  (let [t (or (s:match "^%s*(.-)%s*$") s)
        n (tonumber t)]
    (if n n
        (case t :true true :false false _ t))))

(fn nest [t k v]
  (let [parts []]
    (each [piece (string.gmatch k "[^.]+")]
      (table.insert parts piece))
    (var cur t)
    (for [i 1 (- (length parts) 1)]
      (when (not (. cur (. parts i))) (tset cur (. parts i) {}))
      (set cur (. cur (. parts i))))
    (tset cur (. parts (length parts)) v)))

(fn csv [path]
  (let [out []]
    (with-open [f (io.open path "r")]
      (each [line (f:lines)]
        (let [line (or (line:match "^([^#]*)") "")]
          (when (line:match "%S")
            (let [row []]
              (each [field (string.gmatch line "([^,]+)")]
                (table.insert row (thing field)))
              (table.insert out row))))))
    out))

(fn ready [file]
  (let [d    (if (= (type file) "table") file (Data (csv file)))
        _    (shuffle d.rows)
        half (floor (/ (length d.rows) 2))]
    (values d
            (clone d (slice d.rows 1 (math.min half the.few)))
            (slice d.rows (+ half 1)))))

(fn loadDoc [doc]
  (each [k v (string.gmatch (or doc "") "([%w%.]+)=(%S+)")]
    (nest the k (thing v))))

;; -- formatting -----------------------------------------------------
(fn o [x]
  (case (type x)
    :number (if (= x (floor x))
                (tostring (floor x))
                (fmt (.. "%." (or the.show.decimals 2) "f") x))
    :string x
    :table  (let [parts []]
              (if (. x 1)
                  (each [_ v (ipairs x)] (table.insert parts (o v)))
                  (let [keys []]
                    (each [k _ (pairs x)] (table.insert keys k))
                    (table.sort keys (fn [a b] (< (tostring a) (tostring b))))
                    (each [_ k (ipairs keys)]
                      (table.insert parts (.. k "=" (o (. x k)))))))
              (.. "{" (table.concat parts ", ") "}"))
    _ (tostring x)))

;; -- (1+1) search core ----------------------------------------------
(fn oneplus1 [data mutate accept oracle ?budget ?restart]
  (let [budget  (or ?budget 1000)
        restart (or ?restart 0)
        out []]
    (var h 0) (var best nil) (var best-e 1e32)
    (var s (copy (choice data.rows))) (var e 1e32) (var imp 0)
    (while (< h budget)
      (var done false)
      (each [_ sn (ipairs (mutate s))]
        (when (not done)
          (set h (+ h 1))
          (let [en (oracle sn)]
            (when (accept e en h budget) (set s sn) (set e en))
            (when (< en best-e)
              (set best (copy sn)) (set best-e en) (set imp h)
              (table.insert out [h best-e best]))
            (when (and (> restart 0) (> (- h imp) restart))
              (set s (copy (choice data.rows)))
              (set e 1e32) (set imp h)
              (set done true))))))
    out))

(fn oracle-nearest [data row]
  (let [near (nearest data row)]
    (each [_ col (ipairs data.cols.ys)]
      (tset row col.at (. near col.at)))
    (disty data row)))

(fn last-of [t] (. t (length t)))

;; -- Boot ------------------------------------------------------------
(loadDoc "  --seed=1 --p=2 --few=128 --show.show=30 --show.decimals=2
            --stats.cliffs=0.195 --stats.conf=1.36 --stats.eps=0.35
            --bayes.m=2 --bayes.k=1")
(srand the.seed)

;; -- module exports --------------------------------------------------
{: the : loadDoc
 : rand : srand : choice : choices : sample : shuffle
 : Col : Num : Sym : Cols : Data
 : add : sub : adds : clone
 : mid : mids : mode : spread : entropy : norm
 : minkowski : disty : distx : aha : nearest : wins
 : like : likes
 : pick : picks
 : oneplus1 : oracle-nearest : last-of
 : dinc : thing : nest : csv : ready : o
 : map : keep : sum : copy : slice : min-by : max-by}
