import { defineComponent, computed } from 'https://unpkg.com/vue@3.0.0-rc.10/dist/vue.esm-browser.prod.js';

export const StateDescription = defineComponent({
  name: 'State Description',
  template: '#state-description',
  props: {
    state: Object,
    pType: String,
  },
  setup(props) {
    function stateToDescription({ tag, contents } = {}, pType) {
      switch (tag) {
        case 'Lobby':
          return { text: `Waiting for enemy player to join ...` };
        case 'Setup':
          if (contents.every((setuped) => !setuped)) {
            return { text: `Setup Phase - (Both Players)`, color: 'bg-green-700' };
          }
          const index = pType === 'Owner' ? 0 : 1;
          return contents[index]
            ? { text: `Setup Phase - (Enemy)` }
            : { text: `Setup Phase - (You)`, color: 'bg-green-700' };
        case 'OwnerTurn':
          return pType === 'Owner' ? { text: 'Your Turn', color: 'bg-green-700' } : { text: 'Enemy Turn' };
        case 'GuestTurn':
          return pType === 'Guest' ? { text: 'Your Turn', color: 'bg-green-700' } : { text: 'Enemy Turn' };
        case 'Finished':
          return { text: 'Finished!' };
        case 'Cancelled':
          return { text: 'Cancelled' };
        default:
          return { text: 'Unkown' };
      }
    }

    const data = computed(() => stateToDescription(props.state, props.pType));

    return {
      data,
    };
  },
});
