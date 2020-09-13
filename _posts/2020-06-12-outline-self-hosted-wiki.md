---
layout: post
title: Self Host a Wiki or Knowledge Base for Your Team
author: Sasank Chilamkurthy
---

How is your startup sharing knowledge with the rest of your team?
We've been using slack's `#general` or `#random` channels to make announcements.
We regularly post documents and PPTs in slack channels so that they can be used by other people. We have a channel called `#setup` to post all IT related information like how to login to VPN etc.

But after a few weeks, these docs/notes become super hard to find. As good as slack's search is, you have to precisely know what you're looking for. What we needed was a centralized knowledge base website - something like [Confluence](https://www.atlassian.com/software/confluence)

But Confluence is clunky and slow, and not cheap ($5/user). We experimented with [TiddlyWiki](https://tiddlywiki.com/). It calls itself 'a non-linear personal web notebook'. It's an opensource software which you can host on your servers or AWS. But its non linear organization makes it super unintuitive and confusing.

## Why Outline?

Then, I found [outline](https://www.getoutline.com/)! Outline is similar to TiddlyWiki in that it's opensource and free to self-host. Its UI is a great balance between simplicity of plain text notes and feature creep of Confluence. Login to outline is through your slack - so one less password to remember (or save). You can create private notebooks for a team or just for yourself. You can create a public link of a note so that you can share it with people outside your team - say via email.

<span class="marginnote">
    Outline has great UI
</span>
<img src='https://www.getoutline.com/images/screenshot.png'>


Best part of all of this is that *data doesn't leave your servers* if you self-host it!
We already have a server lying around on AWS to host our own [python package server, pypi](https://en.wikipedia.org/wiki/Python_Package_Index). Since neither hosting pypi nor hosting outline are particularly intensive, we've hosted outline on this machine as `wiki.qure.ai`.

## Install Outline

Unfortunately, documentation for self-hosting outline is limited. There's no robust docker-compose avaialable that you can use to directly create your server. In the rest of this post, I'll show you how to host in your laptop or server. Before starting, make sure to install [docker](https://docs.docker.com/get-docker/) and [docker-compose](https://docs.docker.com/compose/install/).

```
git clone https://github.com/chsasank/outline-wiki-docker-compose.git
cd outline-wiki-docker-compose
make install
```
<span class="marginnote">
    make install
</span>
<img src='/assets/images/outline/make_install.png'>


Follow the instructions. You'll have to create a slack app.
<span class="marginnote">
   Slack app
</span>
<img style="max-width: 75%;" src='/assets/images/outline/slack_app.png'>


If you want to install HTTPS:

```bash
make https
```

Run the server:

```bash
make start
```
