alphagov-backend.hs
===================

Drop-in replacements for [GOV.UK][] backend services, written in
Haskell using [servant][].

This is my firebreak project for end-of-Q1 2018

[GOV.UK]: https://www.gov.uk
[servant]: http://haskell-servant.readthedocs.io


Outline
-------

For a service `$SERVICE`:

- `$SERVICE/api`
  is a library defining its interface
- `$SERVICE/server`
  is an executable providing the server
- `$SERVICE/test`
  is an executable intended to be run against the server

The server and test executables are called `$SERVICE` and
`$SERVICE-test` respectively.

Build everything with `stack`:

    $ stack build

Run a server or testsuite with `stack`:

    $ stack exec $SERVICE &
    $ stack exec $SERVICE-test

Or directly:

    $ ./.stack-work/install/x86_64-linux/lts-11.15/8.2.2/bin/$SERVICE &
    $ ./.stack-work/install/x86_64-linux/lts-11.15/8.2.2/bin/$SERVICE-test


Running in the VM
-----------------

This assumes you are fortunate enough to have a GOV.UK VM.  You don't
even need to work for GDS to have that excitement!  See "Boot your VM"
in the [get started docs][].

[get started docs]: https://docs.publishing.service.gov.uk/manual/get-started.html#4-boot-your-vm

1. Install `stack` in your VM:

    ```bash
    $ curl -sSL https://get.haskellstack.org/ | sh
    ```

2. Copy the code into your VM.

3. Build everything (this will take a little while):
    ```bash
    $ cd /path/to/code
    $ stack build --allow-different-user
    ```

4. Edit `/var/govuk/govuk-puppet/development-vm/Procfile` to use the Haskell binaries instead:
    ```diff
    diff --git a/development-vm/Procfile b/development-vm/Procfile
    index 1a4b7a7e1..a40405b33 100644
    --- a/development-vm/Procfile
    +++ b/development-vm/Procfile
    @@ -39,8 +39,7 @@ support:               govuk_setenv support               ./run_in.sh ../../supp
     travel_advice_publisher: govuk_setenv travel-advice-publisher ./run_in.sh ../../travel-advice-publisher bundle exec rails server -p 3035
     travel_advice_publisher_worker: govuk_setenv travel-advice-publisher ./run_in.sh ../../travel-advice-publisher bundle exec sidekiq -C ./config/sidekiq.yml
     release:               govuk_setenv release               ./run_in.sh ../../release        bundle exec rails server -p 3036
    -asset-manager:         govuk_setenv asset-manager         ./run_in.sh ../../asset-manager  bundle exec rails server -p 3037
    -asset-manager-sidekiq:  govuk_setenv asset-manager         ./run_in.sh ../../asset-manager  bundle exec sidekiq -C ./config/sidekiq.yml
    +asset-manager:         govuk_setenv asset-manager         ./run_in.sh ../../alphagov-backend.hs stack exec --allow-different-user asset-manager
     # asset-manager sidekiq monitoring uses 3038
     # limelight used port 3040
     # transaction_wrappers used port 3041
    ```

Now start things with `bowl` as usual.
