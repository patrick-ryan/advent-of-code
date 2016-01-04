; Advent of Code

(ns advent-of-code-clj.core
	(:gen-class)
	(:require [clojure.contrib.command-line :as ccl]))

; Day 3: Perfectly Spherical Houses in a Vacuum

(defn read-file [f]
	(-> (slurp f)
    	(clojure.string/split-lines)))

(defn new-coordinates [direction dir-index santa-coordinates robo-coordinates]
	(let [coordinates (condp = (even? dir-index)
		true santa-coordinates
		false robo-coordinates)]
		(let [new-coordinates (condp = (str direction)
			"^" [(get coordinates 0) (+ (get coordinates 1) 1)]
			"v" [(get coordinates 0) (- (get coordinates 1) 1)]
			">" [(+ (get coordinates 0) 1) (get coordinates 1)]
			"<" [(- (get coordinates 0) 1) (get coordinates 1)])]
			(condp = (even? dir-index)
				true [new-coordinates new-coordinates robo-coordinates]
				false [new-coordinates santa-coordinates new-coordinates]))))

(defn visited [coordinates visited-list result]
	(if (contains? visited-list coordinates)
		[visited-list result]
		[(conj visited-list coordinates) (inc result)]))

(defn find-houses [directions]
	(let [max-index (count directions)]
		(loop [dir-index 0 santa-coordinates [0 0] robo-coordinates [0 0] visited-list #{[0 0]} result 1]
			(if (= dir-index max-index)
				result
				(let [[new-coordinates c1 c2] (new-coordinates (get directions dir-index) dir-index santa-coordinates robo-coordinates)]
					(let [[new-visited-list new-result] (visited new-coordinates visited-list result)]
						(recur (inc dir-index) c1 c2 new-visited-list new-result)))))))

(defn -main [& args]
	(ccl/with-command-line args
		"Filename"
    	[[filename "Filename" "D:/Dropbox/doc-sync/Programming/advent-of-code/day-3.txt"]]
		(println "Houses: " (find-houses (get (read-file filename) 0)))))