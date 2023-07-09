bash -c 'export $(cat server/.env.example | sed "s/#.*//g" | xargs) && code ./marauders-radar.code-workspace'
