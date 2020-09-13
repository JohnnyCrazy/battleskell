import { createApp, onBeforeMount, ref } from 'https://unpkg.com/vue@3.0.0-rc.10/dist/vue.esm-browser.prod.js';
import { createPingSocket } from './util.mjs';

import { StateDescription } from './game/state-description.mjs';
import { ShipLog } from './game/ship-log.mjs';
import { SetupView } from './game/views/setup.mjs';
import { TurnView } from './game/views/turn.mjs';
import { FinishedView } from './game/views/finished.mjs';

createApp({
  components: { StateDescription, ShipLog, SetupView, TurnView, FinishedView },
  setup() {
    const loading = ref(true);
    const game = ref({});

    createPingSocket(location.host + location.pathname + '/state', (id, val) => {
      if (id === 'state-update') {
        game.value.state = val;
      }
      if (id === 'battlefield-update') {
        game.value.stateView = val;
      }
      if (id === 'log-added') {
        game.value.logs.push(val);
      }
    });

    onBeforeMount(async () => {
      const gameCall = await fetch(location.href + '/state', {
        headers: { accept: 'application/json' },
      });
      game.value = await gameCall.json();
      loading.value = false;
    });

    return {
      game,
      loading,
    };
  },
}).mount('#game');
