import random

class Reset(Exception): pass

def track(f):
    """Manages depth state. Unwinds cleanly even during a Reset."""
    def wrap(self, *args):
        self.d += 1
        if self.d > self.max_d:
            self.d -= 1; raise Reset("Depth")
        try: return f(self, *args)
        finally: self.d -= 1
    return wrap

class Explore:
    def __init__(self): self.reset(1)
    
    def reset(self, max_d):
        self.d, self.bad, self.max_d = 0, 0, max_d

    def ror(self, *choices):
        """Logical OR: Explores randomly. Succeeds if >= 1 is true."""
        paths = list(choices); random.shuffle(paths)
        for p in paths:
            try: return p()
            except Reset: pass
        
        self.bad += 1
        if self.bad > 3: raise Reset("Too many bad choices")
        raise Reset("All ror paths failed")

    def rand(self, *choices):
        """Logical AND: Explores randomly. Succeeds if ALL are true."""
        paths = list(choices); random.shuffle(paths)
        return [p() for p in paths]

    def dfid(self, target, max_limit=10):
        """Iterative Deepening loop. Restarts universe on failure."""
        for limit in range(1, max_limit + 1):
            self.reset(limit)
            try: return target()
            except Reset: pass
        return None

# --- GRAMMAR GENERATOR IMPLEMENTATION ---

class Phone(Explore):
    # Bidirectional constraint mapping (Fail Early Propagation)
    BANS = {
        "GPS": {"Colour", "High res"}, "High res": {"GPS"},
        "Camera": {"Basic", "Colour"}, "Colour": {"GPS", "Camera"},
        "Basic": {"Camera"}
    }

    def reset(self, max_d):
        super().reset(max_d)
        self.feats, self.banned = set(), set()

    @track
    def add(self, feat):
        """The active guard: Fails instantly if feature is shadowed."""
        if feat in self.banned: raise Reset(f"{feat} is banned")
        self.feats.add(feat)
        self.banned.update(self.BANS.get(feat, []))
        return True

    @track
    def build(self):
        self.add("Phone"); self.add("Calls")
        self.rand(self.screen, self.gps, self.media)
        return self.feats

    @track
    def screen(self):
        return self.ror(lambda: self.add("Basic"),
                        lambda: self.add("Colour"),
                        lambda: self.add("High res"))

    @track
    def gps(self):
        return self.ror(lambda: self.add("GPS"), lambda: True)

    @track
    def media(self):
        return self.ror(self.media_feats, lambda: True)

    @track
    def media_feats(self):
        self.add("Media")
        # Inclusive OR (Camera, MP3, or Both)
        return self.ror(
            lambda: self.add("Camera"), 
            lambda: self.add("MP3"),
            lambda: self.rand(lambda: self.add("Camera"), lambda: self.add("MP3"))
        )

# --- EXECUTION ---
p = Phone()
valid_phone = p.dfid(p.build)

print(f"Generated valid configuration: {valid_phone}")
