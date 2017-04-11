<template>
  <chat-messages :messages="messages" :players="this.info.players"></chat-messages>
</template>
<script>
  import {user_channel} from './socket'
  import ChatMessages from './components/ChatMessages'


  export default {
    data() {
      return {
        info: {},
        messages: []
      }
    },
    created() {
      this.load()
    },
    methods: {
      load() {
        user_channel.push("get:archive", {id: this.$route.params.game_id})
          .receive("ok", (d) => {
            console.log(d)
            this.messages = d.msgs
          })
      },
    },
    watch: {
      $route: 'load',
    },
    components: {ChatMessages}
  }
</script>
