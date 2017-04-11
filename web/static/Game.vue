<template>
  <div class="game-room">
    <room-header :name="name"></room-header>
    <div class="room-inner" ref="inner">
      <chat-messages :messages="messages" :players="info.players"></chat-messages>
    </div>
    <div class="game-ui">
      <button v-if="joined" type="button" @click="leaveGame" class="leave-button">
        Leave the game
      </button>
      <button v-else type="button" @click="joinGame" class="leave-button">
        Attempt to join this game
      </button>
      Phase: {{phase}}
      <div class="team" v-if="info.teams && info.teams.length">
        Alignment: {{info.teams.join(", ")}}<br />
      </div>
      <div class="player-list" v-if="info.players">
        <h3>Playerlist</h3>
        <div class="player" v-for="player in playing">
          {{player.name}}
          <span v-if="info.player_status">
            {{player_status(player.slot)}}
          </span>
        </div>
      </div>
      <div class="inactive" v-if="info.inactive && info.inactive.length">
        <h3>Inactive Roles</h3>
        <div class="channel" v-for="channel in roleChannels(info.inactive)">
          <div class="role" v-if="channel.role">
            <b><role :role="channel.role"></role></b>
          </div>
          <div class="players" v-if="channel.members">
            Members: <span class="player" v-for="slot in channel.members">
              {{slotName(slot, info.players)}}
            </span>
          </div>
        </div>
      </div>
      <div class="active" v-if="info.active && info.active.length">
        <h3>Active Roles</h3>
        <div class="channel" v-for="channel in roleChannels(info.active)">
          <div class="role" v-if="channel.role">
            <b><role :role="channel.role"></role></b>
          </div>
          <div class="players" v-if="channel.members">
            <div class="player" v-for="slot in channel.members">
              {{slotName(slot, info.players)}}
              <span class="vote" v-if="voteCount(channel, slot)">
                (<b>{{voteCount(channel, slot)}}</b>):
                {{
                  renderVotedBy(channel, slot) || "no one"
                }}
              </span>
              <div class="small vote" v-if="isVoting(channel, slot)">
                votes to {{
                  renderVote(isVoting(channel, slot), info.players)
                }}
              </div>
            </div>
          </div>
          <div class="act" v-if="channel.actions && channel.actions.length">
            <v-select
              placeholder="Vote"
              :options="channel.actions.map(voteOption)"
              track-by="vote"
              label="label"
              :value="isVoting(channel, me.slot) ? voteOption(isVoting(channel, me.slot)) : {}"
              @input="vote(channel, $event)">
            </v-select>
          </div>
        </div>
      </div>
    </div>
    <form @submit.prevent="send">
      <input type="text" v-model="input"/>
    </form>
    <!-- <chat-input v-if="info.active" :channels="roleChannels(info.active.channels)"></chat-input> -->
  </div>
</template>
<script>
  import {default as socket, queue_channel} from './socket'
  import RoomHeader from './components/RoomHeader'
  import ChatMessages from './components/ChatMessages'
  import ChatInput from './components/ChatInput'
  import {renderVote, slotName} from './textviews'
  import Vote from './components/Vote'
  import Role from './components/Role'
  import Vue from 'vue'

  export default {
    data() {
      return {
        messages: null,
        input: "",
        meets: [],
        info: {},
        joined: true
      }
    },
    created() {
      this.load()
    },
    methods: {
      load() {
        if (this.channel) {
          this.channel.leave()
        }
        this.channel = socket.channel(this.topic)
        this.channel.join()
          .receive("ok", (d) => {
            console.log(d)
            this.messages = d.msgs
            this.handleGameInfo(d)
            this.joined = true
          })
          .receive("error", e => {
            console.log(e)
            this.joined = false
          })

        this.channel.on("new:msg", msg => {
          console.log(msg)
          this.messages.push(msg)
          this.requestGameInfo()
        })
      },
      send() {
        if (this.input) {
          this.activeChannel.push("new:msg", {type: 'm', msg: this.input})
        }
        this.input = ''
      },
      handleGameInfo(d) {
        delete d.msgs

        let newMeets = []

        let meetTopics = d.active
          ? d.active.map(meet => "meet:" + meet.channel)
          : ["talk:" + d.id]

        for (let topic of meetTopics) {
          let existing = this.meets.filter(x => x.topic == topic)[0]
          if (existing) {
            newMeets.push(existing)
          }
          else {
            let channel = socket.channel(topic)

            channel.join().receive("error", e => console.log(e))
            channel.on("new:msg", msg => {
              msg.topic = channel.topic
              this.messages.push(msg)

              if (msg.ty != "m") {
                this.requestGameInfo()
              }
            })

            newMeets.push(channel)
          }
        }

        this.meets
          .filter(x => newMeets.indexOf(x) == -1)
          .map(x => x.leave())

        this.meets = newMeets

        this.activeChannel = this.meets[0]

        this.info = d
      },
      requestGameInfo() {
        this.channel.push("info")
          .receive("ok", g => this.handleGameInfo(g))
          .receive("error", g => console.log(g))
      },
      renderVote,
      slotName,
      voteOption(v) {
        return {
          vote: v,
          label: renderVote(v, this.info.players)
        }
      },
      isVoting(channel, slot) {
        return channel.votes.filter(x => x.player == slot)[0]
      },
      renderVotedBy(channel, slot) {
        return channel.votes
          .filter(x => ~x.opt.indexOf(slot))
          .map(x => slotName(x.player, this.info.players))
          .join(", ")
      },
      voteCount(channel, slot) {
        return channel.votes
          .filter(x => ~x.opt.indexOf(slot))
          .length
      },
      actor(vote) {
        return vote.player
      },
      roleChannels(channels) {
        return channels.filter(c => c.type != "player")
      },
      vote(channel, v) {
        this.meets.filter(x => x.topic == "meet:" + channel.channel)[0]
          .push("new:vote", v.vote)
      },
      player_status(slot) {
        return this.info.player_status.filter(x => x.slot == slot)[0].status
      },
      leaveGame() {
        if (this.info.status == "ongoing" || this.info.status == "signups") {
          queue_channel.push("out", {id: this.$route.params.game_id})
            .receive("ok", _ => this.$router.push("/"))
            .receive("error", _ => console.log(_))
        }
        else {
          this.$router.push("/")
        }
      },
      joinGame() {
        queue_channel.push("signup", {id: this.$route.params.game_id})
          .receive("ok", _ => this.load())
          .receive("error", _ => console.log(_))
      }
    },
    watch: {
      $route: 'load',
      messages() {
        let {inner} = this.$refs
        // almost scrolled down
        if(inner && inner.scrollTop + inner.clientHeight + 60 >= inner.scrollHeight) {
          // scroll all the way down
          Vue.nextTick(_ => inner.scrollTop = inner.scrollHeight)
        }
      }
    },
    components: {RoomHeader, ChatMessages, ChatInput, Vote, Role},
    computed: {
      topic() {
        return `${this.$route.name}:${this.$route.params.game_id}`
      },
      me() {
        return this.info.players.filter(x => x.user == window.user)[0]
      },
      name() {
        return this.info.setup_name
          ? `Game ${this.$route.params.game_id}: ${this.info.setup_name}`
          : `Game ${this.$route.params.game_id}`
      },
      phase() {
        if (this.info.status == "ongoing") {
          return `${this.info.phase.name} ${this.info.phase.number}`
        }
        else {
          return this.info.status
        }
      },
      playing() {
        return this.info.players.filter(x => x.status == "playing")
      }
    }
  }
</script>
<style>
  .game-room {
    position: relative;
    width: 100%;
    height: 100%;
    > .room-inner {
      overflow-y: auto;
      position: absolute;
      width: 70%;
      top: 3.8em;
      border-top: 1px solid silver;
      border-right: 1px solid silver;
      bottom: 2em;
      margin-bottom: 5px;
    }
    > form {
      position: absolute;
      bottom: 0;
      left: 0;
      right: 0;
      display: flex;
      background-color: silver;
      height: 2.4em;
      > input {
        border: 1px solid grey;
        background-color: #fff;
        margin: .4em;
        height: 1.6em;
        padding-left: 5px;
        width: 100%;
      }
    }
  }
  .game-ui {
    overflow-y: auto;
    position: absolute;
    top: 4em;
    right: 0;
    padding: 20px;
    width: 30%;
    bottom: 2em;
    margin-bottom: 5px;
    .leave-button {
      float: right;
    }
  }
</style>
