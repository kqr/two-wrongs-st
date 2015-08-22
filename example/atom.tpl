<?xml version="1.0" encoding="utf-8"?>
<feed xmlns="http://www.w3.org/2005/Atom">
  <title>Blog Title</title>
  <link href="http://www.example.com/" />
  <link rel="self" href="http://www.example.com/atom" />
  <id>http://www.example.com/</id>
  <updated><lastEntry /></updated>
  <author><name>Your Name</name></author>

  <entry>
    <latestEntries>
      <title><entryTitle /></title>
      <author><name>Your Name</name></author>
      <id>http://www.example.com/<entrySlug /></id>
      <updated><datestamp /></updated>
      <link rel="alternate" href="http://www.example.com/${entrySlug}"></link>
    </latestEntries>
  </entry>

