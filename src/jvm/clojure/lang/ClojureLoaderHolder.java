package clojure.lang;

import java.util.concurrent.atomic.AtomicReference;

/**
 * @author Colin Fleming
 */
public class ClojureLoaderHolder {
  public static AtomicReference<ClassLoader> loader = new AtomicReference<>(null);
}
