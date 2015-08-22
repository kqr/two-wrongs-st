<?xml version="1.0" encoding="utf-8"?>
<feed xmlns="http://www.w3.org/2005/Atom">
  <title><blogName /></title>
  <link href="${blogURL}" />
  <link rel="self" href="${blogURL}atom" />
  <id><blogURL /></id>
  <updated><lastEntry /></updated>
  <author><name>Your Name</name></author>

  <entry>
    <latestPosts>
      <title><entryTitle /></title>
      <author><name>Your Name</name></author>
      <id><blogURL /><entrySlug /></id>
      <updated><timestamp /></updated>
      <link href="${blogURL}${entrySlug}" />
      <content><escapedContent /></content>
    </latestPosts>
  </entry>
</feed>
