import Scorm

main :: IO ()
main = do
  -- Test helloWorld
  if helloWorld == "Hello, World!"
    then putStrLn "✓ helloWorld test passed"
    else error "helloWorld test failed"
  
  -- Test add function
  if add 2 3 == 5
    then putStrLn "✓ add test passed"
    else error "add test failed"
  
  if add 0 0 == 0
    then putStrLn "✓ add zero test passed"
    else error "add zero test failed"
  
  putStrLn "All tests passed!"

