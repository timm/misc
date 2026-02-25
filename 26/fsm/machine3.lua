local machine = require"fsm3"

-- 1. Closure: returns an action function
local function say(msg)
  return function(p)
    print(string.format("[%s] %s", p.name,
      msg:gsub("{hp}", p.hp)))
  end
end

-- 2. Semantic helpers
local function pop_damage(p)
  return table.remove(p.damage_queue, 1) or 0 end
local function inject(p, e)
  table.insert(p.queue, 1, e) end

-- 3. Composer: runs several actions in sequence
local function combine(...)
  local fns = {...}
  return function(p)
    for _, f in ipairs(fns) do f(p) end end
end

-- 4. Mixin: merge common transitions into every state
local common = { hit = "staggered", die = "dead" }
local function mix(t)
  for k, v in pairs(common) do t[k] = v end
  return t
end

-- ---- rules ----------------------------------------
local rules = {
  idle = {
    action      = say("is idling. HP: {hp}"),
    transitions = mix{ walk="moving", attack="attacking" }
  },
  moving = {
    action      = say("is walking forward."),
    transitions = mix{ stop="idle", attack="attacking" }
  },
  attacking = {
    action      = say("swings their weapon!"),
    transitions = mix{ recover="idle" }
  },
  staggered = {
    action = combine(
      function(p) p.hp = p.hp - pop_damage(p) end,
      say("took damage! HP: {hp}"),
      function(p)
        if p.hp <= 0 then inject(p, "die") end end
    ),
    transitions = mix{ recover="idle" }
  },
  dead = {
    action      = say("has collapsed."),
    transitions = { revive="idle" }
  }
}

-- ---- payload --------------------------------------
local hero = {
  name = "Hero", hp = 100,
  queue = {
    "walk","attack","recover",
    "hit","recover",
    "walk","attack",
    "hit","walk"
  },
  damage_queue = { 15, 90 }
}

print("=== STARTING ===")
local m = machine.start(rules, "idle", hero)
print("\n=== DONE ===")
print("Queue left: " .. #m.queue)
print("Final HP:   " .. m.hp)
