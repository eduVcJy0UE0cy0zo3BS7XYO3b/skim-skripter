;;; Copyright (C) 2024 Dmitry Polyakov <liltechdude@gmail.com>
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; Commentary:
;;;
;;; Madness begins there.
;;;
;;; Code:

(define-module (demo assets)
  #:use-module (dom image)
  #:use-module (dom media)
  #:export (image:end
	    image:kitchen
	    image:nastya-tired
	    image:nastya-normal
	    image:nastya-normal-ice
	    image:masha-happy
	    image:masha-normal
	    image:masha-troubled
	    audio:curious_critters))

(define (image:end)
  (make-image "resources/bg/end.png"))

(define (image:kitchen)
  (make-image "resources/bg/bg_kitchen.jpeg"))

(define (image:nastya-tired)
  (make-image "resources/sprites/nastya_tired_1600.webp"))

(define (image:nastya-normal)
  (make-image "resources/sprites/nastya_normal_1600.png"))

(define (image:nastya-normal-ice)
  (make-image "resources/sprites/nastya_normal_1600_ice.png"))

(define (image:masha-happy)
  (make-image "resources/sprites/masha_happy_1600.webp"))

(define (image:masha-normal)
  (make-image "resources/sprites/masha_normal_1600.png"))

(define (image:masha-troubled)
  (make-image "resources/sprites/masha_troubled_1600.png"))

(define (audio:curious_critters)
  (make-audio "resources/bgm/curious_critters_extended.mp3"))
