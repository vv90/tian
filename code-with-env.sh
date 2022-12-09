bash -c 'export $(cat server/.env.example | sed "s/#.*//g" | xargs) && code ./contest-viewer.code-workspace'
