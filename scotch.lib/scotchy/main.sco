import scotchy.parse
import scotchy.eval
import scotchy.substitute

main = do user_input := input;
          case user_input of
            "quit": skip,
            otherwise: do parsed := parse(user_input);
                          result := eval(parsed);
                          print user_input;
                          main;
          ;

main
