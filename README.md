# Dr Robotnik web scraper robot factory

Dr Robotnik web scraper robot factory is a minimal framework for writing
web scraping robots.

Included is the behaviour `gen_robot`, for implementing scraper robots,
and a minimal framework for spawning and handling such scraper robots.


## Architecture

Each scraper robot implements `gen_robot` which means implementing
initializing code (usually setup and login), kill code (usually cleanup
and logout), and a sequence of attacks (the actual scraping attempts).
The attacks can be any Erlang code but usually is a mix of HTTP requests
and extraction of information; which are either returned as result or
fed to the next attack.

The robotnik application has a `robot_zoo` which is responsible for
spawning and handling the implemented scraper robots.

The main module for interaction is `robotnik` which includes helpers for
HTTP interaction and cookie handling. Although it is possible to just
use Erlang code the `robotnik` helpers are recommended.

*NB! Each web scraper robot needs to handle it's own (session) cookies.*

*NB! The robotnik:run_robot/2 call is blocking.*


## Example

See `examples/robot_example.erl` for a working example. Run the example
scraper robot simply by invoking

    {ok, Result} =
        robotnik:run_robot(robot_example, [{credentials, {User, Password}}]).

This is how you would execute your home rolled scraper robots as well,
supplying a robot module and a initialization argument list.


## License

Dr Robotnik web scraper robot factory is released under an MIT license.
See LICENSE for the full license text.


  vim:ft=markdown:tw=72
