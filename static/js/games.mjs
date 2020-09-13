import { createApp, ref, onMounted, onUnmounted } from 'https://unpkg.com/vue@3.0.0-rc.10/dist/vue.esm-browser.prod.js';
import { createPingSocket } from './util.mjs';

createApp({
  setup() {
    const games = ref([]);

    onMounted(async () => {
      const test = await fetch(location.href, {
        headers: { accept: 'application/json' },
      });
      games.value = await test.json();
    });

    createPingSocket(location.host + location.pathname, (id, val) => {
      if (id === 'lobby-added') {
        games.value.push(val);
      } else if (id === 'lobby-removed') {
        games.value = games.value.filter(({ game }) => game.id !== val);
      }
    });

    return {
      games,
    };
  },
}).mount('#game-table');
