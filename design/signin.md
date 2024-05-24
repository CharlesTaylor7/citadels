Not planning to support Email + Password logins. Supabase comes with a very basic SMTP server that only can send 3 emails per hours. Since I'm trying to do this with zero spend, I'm also going to avoid using something like Twilio for sending email confirmations and password resets.

Instead we're just going to use Social OAuth providers.

To start I've integrated with Discord, but theoretically dropping in other OAuth providers would be pretty straightforward.

In the future allow:
- Signin with Tumblr


I did try to a trigger working that would insert a default username for each user after signin, but it wasn't working and was painful to debug. 

I realized though its better design to allow users to change their usernmae before its made publicly available to others.

So now successful signin will redirect to the /lobby page if they already have a profile and to /profile if they don't.
