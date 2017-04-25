# simple-reader

simple-reader is a node.js process that downloads your feeds, and a web client to access them.
Made with Rum (React wrapper), Specter, feedparser. Checkout project.clj & package.json for full dependencies and credits.

## Web interface

The interface is split in two, the list of your feed subscriptions on the left, the feed you are reading on the right.

### Subscription view:

Feeds are listed under their tag to help you organise them (news, tech, comics...). 
Click on the orange triangle of a tag to hide/show its content.
Click on the grey one to hide/show feeds with no unread articles, which are hidden by default (try this on Comics).

### Feed/Article view:

Click on the article you want to read.
An orange article is unread, green is saved, grey is read. Read articles are deleted once they're dropped from the rss feed.
Top right controls to change the feed settings (article order, show unread or not...) are disabled in the demo.

### Keyboard navigation:

- j: next article
- k: previous article (takes you to the last one if no articles are opened)
- m: toggle mark as (un)read
- s: toggle mark as saved
- g: go to the top of the feed
- G: go to the bottom of the feed
- r: refresh current feed
- R: refresh all feeds
- v: open article in new tab
- f: toggle fullscreen
- b: open feed search, type the name of a feed (try "gua" to select "The Guardian"), once you find it hit enter. Use up and down arrows to navigate through search results

## Source

Checkout the source code on [github](https://github.com/Wonko7/sreader).
