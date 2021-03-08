# Troubleshooting
Find the appropriate error message for a description of how to solve it.

## unbuildable library 'library' while running `stack test`

Error:      
    Dependency on unbuildable library 'library' from phonetics-modeling


### Solution: upgrade stack to latest version
You already have stack installed, but it is not the correct version.
Run the following command so that stack upgrades itself.

`stack upgrade --force-download`
