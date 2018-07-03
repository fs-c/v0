#pragma once
#include "Array.h"
#include "Entity.h"
#include "Offsets.h"

namespace Engine
{
	class GameManager
	{
	public:
		Array<Entity*> GetEntities();
		static GameManager* GetInstance();
		Entity* GetLocalPlayer();
		bool IsInGame();
		uintptr_t* GetAddress(uintptr_t address);
	};
}