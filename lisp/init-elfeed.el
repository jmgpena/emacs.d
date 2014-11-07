;;; init-elfeed.el --- package stuff
;;; Commentary:
;;;
;;; By jmgpena
;;;
;;; Code:

;; elfeed configuration
(require-package 'elfeed)
(setq elfeed-db-directory "~/Dropbox/org/elfeed"
      elfeed-sort-order 'ascending
      elfeed-feeds
      '("http://nullprogram.com/feed"
        "http://feed.dilbert.com/dilbert/daily_strip"
        "http://maeclaudia.blogspot.com/feeds/posts/default"
        "http://roguebase.net/feed-rss.xml"
        "http://feeds.feedburner.com/little-gamers/XrRa"
        "http://feeds.feedburner.com/AbstruseGoose"
        "http://feeds.feedburner.com/buttersafe"
        "http://www.giantitp.com/comics/oots.rss"
        "http://feeds.feedburner.com/Explosm"
        "http://www.exocomics.com/feed"
        "http://pbfcomics.com/feed/feed.xml"
        "http://popstrip.com/rss/"
        "http://feeds.feedburner.com/SpikedMath"
        "http://www.xkcd.com/rss.xml"
        "http://www.questionablecontent.net/QCRSS.xml"
        "http://www.sinfest.net/rss.php"
        "http://reallifecomics.com/rss.php?feed=rss2"
        "http://feeds.feedburner.com/OpenCulture"
        "http://feeds.feedburner.com/brainpickings/rss"
        "http://feeds.feedburner.com/zenhabits"
        "http://pipes.yahoo.com/pipes/pipe.run?_id=2620de78bcda86952188892c48b4caf1&_render=rss"
        "http://planet.emacsen.org/atom.xml"
        "http://endlessparentheses.com/atom.xml"
        "http://aarontgrogg.com/feed/"
        "http://onethingwell.org/rss"
        "http://sachachua.com/blog/feed"
        "http://www.echojs.com/rss"))
(global-set-key (kbd "C-x w") 'elfeed)

(provide 'init-elfeed)
;;; init-elfeed.el ends here
