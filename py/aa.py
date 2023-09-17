from dataclasses import dataclass,field
from typing import List

@dataclass
class C:
    name: str = 'timm'
    unit_price: float = 23
    quantity_on_hand: int = 0

@dataclass
class D(C):
    fred : List[C] =None

print(D())
