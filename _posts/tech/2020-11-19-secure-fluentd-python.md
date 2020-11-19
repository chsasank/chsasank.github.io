---
layout: post
title: How to use Fluentd for Python Logging
author: Sasank Chilamkurthy
---

Fluentd is a unified data collector for logging. It allows you to collect logs from wide variety of sources and save them to different places like S3, mongodb etc. It is fairly lightweight and allows integration to any app fairly easy. In this post, let's see how to spin up a fluent server and forward logs to another fluent server. The logs will be pushed to the fluent server using python.

## Security

Create certificate and private key for TLS encryption. You'll be prompted for a password. I used `sasank` for illustrative purposes.

```
$ openssl req -new -x509 -sha256 -days 1095 -newkey rsa:2048 \
              -keyout fluentd.key -out fluentd.crt
Generating a RSA private key
..+++++
............+++++
writing new private key to 'fluentd.key'
Enter PEM pass phrase:
Verifying - Enter PEM pass phrase:
-----
You are about to be asked to enter information that will be incorporated
into your certificate request.
What you are about to enter is what is called a Distinguished Name or a DN.
There are quite a few fields but you can leave some blank
For some fields there will be a default value,
If you enter '.', the field will be left blank.
-----
Country Name (2 letter code) [AU]:
State or Province Name (full name) [Some-State]:
Locality Name (eg, city) []:
Organization Name (eg, company) [Internet Widgits Pty Ltd]:
Organizational Unit Name (eg, section) []:
Common Name (e.g. server FQDN or YOUR name) []:
Email Address []:
```

Set permission to the generated certificate and key:

```
chmod 700 fluentd.crt
chmod 400 fluentd.key
```

We want to mount these files to docker. Add the following lines to volume section of target fluent service.

```
- ./fluentd.crt:/etc/certs/fluentd.crt
- ./fluentd.key:/etc/certs/fluentd.key
```

And add the following configuration to `fluentd_target.conf`

```
<source>
  @type forward
  port 24224
  bind 0.0.0.0
  <transport tls>
    cert_path /etc/certs/fluentd.crt
    private_key_path /etc/certs/fluentd.key
    private_key_passphrase sasank
  </transport>
</source>

<match *.*>
  @type stdout
</match>
```
That's it our target fluent is now TLS ready. Now let's configure source fluent to send TLS encrypted data. Since we generated self-signed certificate we have to mount it to our source fluent docker. Make sure following line is present in volumes section of `sourcefluent` docker compose.

```
- ./fluentd.crt:/etc/certs/fluentd.crt
```

Now adjust the configuration of source fluent: `fluentd_source.conf`

```
<source>
  @type forward
  port 24224
  bind 0.0.0.0
</source>

<match *.*>
  @type copy
  <store>
    @type stdout
  </store>
  <store>
    @type forward
    transport tls
    tls_cert_path /etc/certs/fluentd.crt
    tls_verify_hostname false  # Set false to ignore cert hostname.
    <server>
      host targetfluent
      port 24224
    </server>
  </store>
</match>
```

That's it! We've setup TLS encryption for all packets being sent from sourcefluent to targetfluent. Note that this is not authentication. We need to setup a password so that only people with that password should be able to send logs to `targetfluent`.

Just add the following lines to @forward section of source and target respectively

```
<security>
    self_hostname sourcefluent
    shared_key my_secure_password
</security>
```

```
<security>
    self_hostname targetfluent
    shared_key my_secure_password
</security>
```

Of course, replace `my_secure_password` with a secure password you can share to the client.