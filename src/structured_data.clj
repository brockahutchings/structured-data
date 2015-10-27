(ns structured-data)

(defn do-a-thing [x]
  (let [x (+ x x)]
    (Math/pow x x)))

(defn spiff [v]
  (let [x (if (get v 0) (get v 0) 0)
        y (if (get v 2) (get v 2) 0)]
  (+ x y)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (- y2 y1)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [pointx pointy] point]
    (and (<= x1 pointx x2) (<= y1 pointy y2))))

(defn contains-rectangle? [outer inner]
 (let [[x y] inner]
    (and (contains-point? outer x)
         (contains-point? outer y))))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  ( > (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (conj (get book :authors) new-author)]
  (assoc book :authors authors)))

(defn alive? [author]
  (nil? (get author :death-year)))

(defn element-lengths [collection]
  (let [countelement (fn [x] (count x))]
  (map countelement collection)))

(defn second-elements [collection]
  (map second collection))

(defn titles [books]
  (let [name (fn [book] (get book :title))]
    (map name books)))

(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count(set a-seq)) (count a-seq)))

(defn old-book->new-book [book]
  (assoc book :authors (set (get book :authors))))

(defn has-author? [book author]
  (contains? (get book :authors) author))

(defn authors [books]
  (let [authors (fn [book] (get book :authors))]
    (apply clojure.set/union (map authors books))))

(defn all-author-names [books]
  (let [author-name (fn [author] (get author :name))]
    (set (map author-name (authors books)))))

(defn author->string [author]
  (let [years (if (contains? author :birth-year)
              (str " (" (get author :birth-year) " - " (get author :death-year) ")")
              )]
  (str (get author :name) years)))

(defn authors->string [authors]
  (let [authorAsString (fn [author] (author->string author))]
    (apply str (interpose ", " (map authorAsString authors)))))

(defn book->string [book]
  (str (get book :title) ", written by " (authors->string (get book :authors))))

(defn books->string [books]
  (if (empty? books)
    (str "No books.")

     (str (if (= 1 (count books))
      (str "1 book. " (apply str (interpose ". " (map book->string books))) ".")
      (str (count books) " books. " (apply str (interpose ". " (map book->string books))) ".")))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn[x] (= name (:name x))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))
; %________%
