module Web.View.Static.StyleGuide where

import Web.View.Prelude
import Web.Element.ElementBuild
import Web.Element.ElementWrap



data StyleGuideView = StyleGuideView
 

instance View StyleGuideView where
  html StyleGuideView =
    [hsx|
      <div class="min-h-screen bg-cultured-100">
         <div class="grid grid-cols-1 sm:grid-cols-2 md:grid-cols-3 lg:grid-cols-4 xl:grid-cols-5 2xl:grid-cols-6 items-center justify-items-center gap-4 p-4">
            {profiles}
         </div>
    </div>
    |]
    where 
      profiles = forEach profilesData renderProfilePage


renderProfilePage :: Profile -> Html
renderProfilePage (Profile name img title role) = 
  [hsx|
    <div class="max-w-[21.5rem] min-w-[15rem] w-full flex-grow divide-y divide-gray-200 rounded-lg bg-white drop-shadow">
        <div class="flex flex-col items-center">
            <img class="h32 mt-8 block w-32 rounded-full" src={img} alt="" />
            <p class="mt-10 text-sm font-medium leading-5 text-gray-900">{name}</p>
            <p class="my-2 text-sm font-normal leading-5 text-gray-500">{title}</p>
            <span class="rounded-full bg-green-100">
                <p class="mx-2 my-1 text-xs font-medium text-green-800">{role}</p>
            </span>
        </div>
        <div class="mt-8 grid h-14 grid-cols-2 divide-x divide-gray-200">
            <button>
                <p class="text-sm font-medium leading-5 text-gray-700">
                    <i class="fas fa-envelope before:mx-2 before:inline-block before:scale-125 before:text-gray-400"></i>
                    Email
                </p>
            </button>
            <button>
                <p class="text-sm font-medium leading-5 text-gray-700">
                    <i class="fas fa-phone before:mx-2 before:inline-block before:rotate-90 before:scale-125 before:text-gray-400"></i>
                    Call
                </p>
            </button>
        </div>
    </div>
  |]



data Profile = Profile {name :: Text, img :: Text, title :: Text, role :: Text}

profilesData :: [Profile]
profilesData = const p <$> [1..10] where
                p = Profile { name= "Jane Copper", img = "https://images.unsplash.com/photo-1472099645785-5658abf4ff4e?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=facearea&facepad=2&w=256&h=256&q=80", title="Paradigm Representative", role = "Admin"}
