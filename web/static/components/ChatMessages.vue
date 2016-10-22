<template>
	<div class="messages">
		<div class="message" v-for="message in messages">
			{{message.msg}}
		</div>
	</div>
</template>
